-- Dynamic Context Pruning (dcp) for maki, ported from @tarquinen/opencode-dcp.
--
-- maki has no outgoing-transcript transform hook, so this uses the seam it
-- does expose: every tool result is filtered by a `tool.*.output` layer before
-- it enters history. Deduplication and context-pressure trimming therefore
-- apply to new output; already-stored output is left to maki's own compaction.
--
-- Configure after require("dcp") with dcp.setup({ ... }); see D below.

local M = {}

local D = {
  enabled = true,
  debug = false,
  allow_subagents = true,
  manual_mode = false,
  automatic_strategies = true,
  min_context_limit = 50000,
  max_context_limit = 100000,
  soft_max_lines = 400,
  soft_max_bytes = 16000,
  hard_max_lines = 120,
  hard_max_bytes = 6000,
  always_max_bytes = 0,
  protected_tools = {
    "todo_write", "todowrite", "todo_read", "todoread", "skill", "task",
    "batch", "question", "write", "edit", "memory", "plan_enter", "plan_exit",
    "compress",
  },
  protected_file_patterns = {},
  protected_kinds = {},
  strategies = {
    deduplication = { enabled = true, mode = "identical", protected_tools = {} },
    purge_errors = { enabled = true, keep_lines = 3, min_bytes = 2000, protected_tools = {} },
  },
  prompt_hint = true,
  persist_stats = true,
  max_seen = 2000,
}

local function merge(dst, src)
  for k, v in pairs(src or {}) do
    if type(v) == "table" and type(dst[k]) == "table" then
      merge(dst[k], v)
    else
      dst[k] = v
    end
  end
end

local cfg = maki.deepcopy(D)

function M.setup(opts)
  merge(cfg, opts)
  return cfg
end

local PK = "[dcp:"

local function tok(n)
  return math.floor(n / 4)
end

local function dbg(msg)
  if cfg.debug then
    maki.log.info("dcp: " .. msg)
  end
end

-- Glob matcher for *, ** and ?.
local function gm(s, i, p, j)
  while j <= #p do
    local c = p:sub(j, j)
    if c == "*" then
      local k = j
      while p:sub(k, k) == "*" do
        k = k + 1
      end
      if p:sub(k, k) == "/" then
        if gm(s, i, p, k + 1) then
          return true
        end
        for x = i, #s do
          if s:sub(x, x) == "/" and gm(s, x + 1, p, k + 1) then
            return true
          end
        end
        return false
      end
      for x = i, #s + 1 do
        if gm(s, x, p, k) then
          return true
        end
      end
      return false
    elseif c == "?" then
      if i > #s then
        return false
      end
      i, j = i + 1, j + 1
    else
      if s:sub(i, i) ~= c then
        return false
      end
      i, j = i + 1, j + 1
    end
  end
  return i > #s
end

local function glob(s, p)
  if type(s) ~= "string" or type(p) ~= "string" or p == "" then
    return false
  end
  return gm((s:gsub("\\", "/")), 1, (p:gsub("\\", "/")), 1)
end

local function any(v, patterns)
  for _, p in ipairs(patterns or {}) do
    if v == p or glob(v, p) then
      return true
    end
  end
  return false
end

-- Stable, key-sorted encoding used for call signatures.
local function canon(v, seen)
  local t = type(v)
  if t == "table" then
    seen = seen or {}
    if seen[v] then
      return '"<cycle>"'
    end
    seen[v] = true
    local out = {}
    if #v > 0 then
      for i = 1, #v do
        out[#out + 1] = canon(v[i], seen)
      end
      seen[v] = nil
      return "[" .. table.concat(out, ",") .. "]"
    end
    local ks = {}
    for k in pairs(v) do
      ks[#ks + 1] = tostring(k)
    end
    table.sort(ks)
    for _, k in ipairs(ks) do
      out[#out + 1] = maki.json.encode(k) .. ":" .. canon(v[k], seen)
    end
    seen[v] = nil
    return "{" .. table.concat(out, ",") .. "}"
  elseif t == "string" then
    return maki.json.encode(v)
  elseif t == "number" or t == "boolean" then
    return tostring(v)
  end
  return "null"
end

local function hash(s)
  local h = 5381
  for i = 1, #s do
    h = (h * 33 + s:byte(i)) % 2147483647
  end
  return h
end

local function split(s)
  local t = {}
  for l in (s .. "\n"):gmatch("(.-)\n") do
    t[#t + 1] = l
  end
  return t
end

local function truncate(s, max_lines, max_bytes)
  if max_lines <= 0 or max_bytes <= 0 then
    return nil
  end
  local L, b = split(s), #s
  if #L <= max_lines and b <= max_bytes then
    return nil
  end
  if #L <= max_lines then
    local h = math.floor(max_bytes * 0.7)
    while h > 0 and s:byte(h + 1) and s:byte(h + 1) >= 0x80 and s:byte(h + 1) < 0xC0 do
      h = h - 1
    end
    local t = math.max(h + 1, b - (max_bytes - h) + 1)
    while t <= b and s:byte(t) >= 0x80 and s:byte(t) < 0xC0 do
      t = t + 1
    end
    return s:sub(1, h)
      .. "\n"
      .. PK
      .. " "
      .. (b - h - (b - t + 1))
      .. " bytes omitted]\n"
      .. s:sub(t)
  end
  local hn = math.max(1, math.floor(max_lines * 0.7))
  local tn = max_lines - hn
  local head, hb = {}, 0
  for i = 1, hn do
    local l = L[i]
    if hb + #l > max_bytes * 0.7 then
      break
    end
    head[#head + 1] = l
    hb = hb + #l + 1
  end
  local tail, tb = {}, 0
  for i = #L, #L - tn + 1, -1 do
    local l = L[i]
    if tb + #l > max_bytes * 0.3 then
      break
    end
    table.insert(tail, 1, l)
    tb = tb + #l + 1
  end
  local omitted = #L - #head - #tail
  if omitted <= 0 then
    return nil
  end
  return table.concat(head, "\n")
    .. "\n"
    .. PK
    .. " "
    .. omitted
    .. " lines omitted]\n"
    .. table.concat(tail, "\n")
end

local KEYS = { "calls", "pruned", "duplicates", "truncated", "errors", "bytes_saved" }

local function zero()
  local t = {}
  for _, k in ipairs(KEYS) do
    t[k] = 0
  end
  return t
end

local totals = zero()
local S = {}

local function st(id)
  id = id or "default"
  local s = S[id]
  if not s then
    s = { id = id, turn = 0, poll = 0, seen = {}, order = {}, stats = zero() }
    S[id] = s
  end
  return s
end

local function limit(v, w)
  if type(v) == "number" then
    return v
  end
  local p = type(v) == "string" and v:match("^(%d+)%%$")
  if p and w and w > 0 then
    return math.floor(w * tonumber(p) / 100)
  end
  return nil
end

local function pressure(s)
  local now = os.time()
  if s.poll == 0 or now - s.poll >= 2 then
    s.poll = now
    local ok, snap = pcall(maki.session.read, { session = s.id })
    if ok and type(snap) == "table" then
      s.cs = snap.context_size or s.cs
      s.cw = snap.context_window or s.cw
    end
  end
  if not s.cs or not s.cw or s.cw <= 0 then
    return "none"
  end
  local hi, lo = limit(cfg.max_context_limit, s.cw), limit(cfg.min_context_limit, s.cw)
  if hi and s.cs >= hi then
    return "hard"
  elseif lo and s.cs >= lo then
    return "soft"
  end
  return "none"
end

local function paths(input)
  local p = {}
  local function add(v)
    if type(v) == "string" and v ~= "" then
      p[#p + 1] = v
    end
  end
  if type(input) ~= "table" then
    return p
  end
  add(input.filePath)
  add(input.file_path)
  add(input.path)
  if type(input.edits) == "table" then
    for _, e in ipairs(input.edits) do
      if type(e) == "table" then
        add(e.filePath)
        add(e.path)
      end
    end
  end
  if type(input.patchText) == "string" then
    for x in input.patchText:gmatch("%*%*%* [ADU][a-z]+ File: ([^\n\r]+)") do
      add(x:match("^%s*(.-)%s*$"))
    end
  end
  return p
end

local function protected(ctx)
  local tool = ctx.tool
  if type(tool) ~= "string" then
    return true
  end
  local s = cfg.strategies
  if
    any(tool, cfg.protected_tools)
    or any(tool, s.deduplication.protected_tools)
    or any(tool, s.purge_errors.protected_tools)
    or (ctx.tool_kind and any(ctx.tool_kind, cfg.protected_kinds))
  then
    return true
  end
  for _, p in ipairs(paths(ctx.input)) do
    if any(p, cfg.protected_file_patterns) then
      return true
    end
  end
  return false
end

local function stats_path()
  local ok, dir = pcall(maki.env.state_dir)
  return ok and dir and maki.fs.joinpath(dir, "dcp", "stats.json") or nil
end

local function load_stats()
  if not cfg.persist_stats then
    return
  end
  local p = stats_path()
  if not p or not maki.fs.metadata(p) then
    return
  end
  local text = maki.fs.read(p)
  local ok, v = pcall(maki.json.decode, text or "")
  if ok and type(v) == "table" then
    for _, k in ipairs(KEYS) do
      totals[k] = math.max(totals[k] or 0, tonumber(v[k]) or 0)
    end
  end
end

local function save_stats()
  if not cfg.persist_stats then
    return
  end
  local p = stats_path()
  if not p then
    return
  end
  local ok, encoded = pcall(maki.json.encode, totals)
  if ok then
    pcall(maki.fs.mkdir, maki.fs.dirname(p), { parents = true })
    pcall(maki.fs.write, p, encoded)
  end
end

local function bump(s, before, after, kind)
  local saved = #before - #after
  if saved <= 0 then
    return
  end
  s.stats.pruned = s.stats.pruned + 1
  s.stats.bytes_saved = s.stats.bytes_saved + saved
  totals.pruned = totals.pruned + 1
  totals.bytes_saved = totals.bytes_saved + saved
  if kind == "duplicate" then
    s.stats.duplicates = s.stats.duplicates + 1
    totals.duplicates = totals.duplicates + 1
  elseif kind == "error" then
    s.stats.errors = s.stats.errors + 1
    totals.errors = totals.errors + 1
  else
    s.stats.truncated = s.stats.truncated + 1
    totals.truncated = totals.truncated + 1
  end
  dbg(kind .. " saved " .. saved .. " bytes")
end

local function dedupe(s, out, ctx)
  local d = cfg.strategies.deduplication
  if not d.enabled or type(out.text) ~= "string" then
    return false
  end
  local key = tostring(hash(tostring(ctx.tool) .. "\1" .. canon(ctx.input or {}))) .. ":" .. #out.text
  if d.mode == "identical" then
    key = key .. ":" .. hash(out.text)
  end
  if s.seen[key] then
    local before = out.text
    out.text = PK .. " duplicate " .. ctx.tool .. " output omitted; use the earlier result]"
    bump(s, before, out.text, "duplicate")
    return true
  end
  s.seen[key] = true
  s.order[#s.order + 1] = key
  if #s.order > cfg.max_seen then
    s.seen[table.remove(s.order, 1)] = nil
  end
  return false
end

local function purge(s, out, level)
  local p = cfg.strategies.purge_errors
  if level == "none" or not p.enabled or not out.is_error then
    return
  end
  local text = out.text
  if type(text) ~= "string" or #text < p.min_bytes then
    return
  end
  local L = split(text)
  if #L <= p.keep_lines + 1 then
    return
  end
  local keep = {}
  for i = 1, math.min(p.keep_lines, #L) do
    keep[i] = L[i]
  end
  keep[#keep + 1] = PK .. " errored output truncated, " .. (#L - #keep - 1) .. " lines omitted]"
  keep[#keep + 1] = L[#L]
  out.text = table.concat(keep, "\n")
  bump(s, text, out.text, "error")
end

local function trim(s, out, level)
  if type(out.text) ~= "string" then
    return
  end
  local text = out.text
  local replacement
  if level == "none" then
    if cfg.always_max_bytes > 0 then
      replacement = truncate(text, math.huge, cfg.always_max_bytes)
    end
  else
    local hard = level == "hard"
    replacement = truncate(
      text,
      hard and cfg.hard_max_lines or cfg.soft_max_lines,
      hard and cfg.hard_max_bytes or cfg.soft_max_bytes
    )
  end
  if replacement then
    out.text = replacement
    bump(s, text, replacement, "truncate")
  end
end

local function on_output(prev, out, ctx)
  if
    not cfg.enabled
    or type(out) ~= "table"
    or ctx.output_mode == "full"
    or (not cfg.allow_subagents and ctx.task_id and ctx.task_id ~= "main")
    or (cfg.manual_mode and not cfg.automatic_strategies)
    or protected(ctx)
  then
    return prev(out, ctx)
  end
  local s = st(ctx.session_id)
  s.stats.calls = s.stats.calls + 1
  totals.calls = totals.calls + 1
  if not dedupe(s, out, ctx) then
    local level = pressure(s)
    purge(s, out, level)
    trim(s, out, level)
  end
  return prev(out, ctx)
end

maki.api.create_autocmd({ "TurnEnd", "AutoCompacting", "CompactionDone" }, {
  callback = function(ev)
    local d = ev.data or {}
    local s = st(d.session_id)
    s.cs = d.context_size or d.context_size_after or s.cs
    s.cw = d.context_window or s.cw
    s.poll = os.time()
    if ev.event == "TurnEnd" then
      s.turn = s.turn + 1
      if s.turn % 5 == 0 then
        save_stats()
      end
    end
  end,
})

maki.api.create_autocmd({ "SessionEnd", "SessionReset" }, {
  callback = function(ev)
    if ev.event ~= "SessionEnd" and ev.event ~= "SessionReset" then
      return
    end
    save_stats()
    local id = (ev.data or {}).session_id
    if id then
      S[id] = nil
    end
  end,
})

maki.api.set_slot("tool.*.output", on_output)

local function hint()
  if not cfg.prompt_hint or not cfg.enabled then
    return ""
  end
  local lines = {
    "Context pruning (dcp) is active: tool results may be shortened or replaced with [dcp: ...] placeholders.",
    "A placeholder means content was omitted to save context; re-run the tool with a narrower query if needed.",
  }
  local id = maki.session.current()
  local s = id and S[id]
  if s and pressure(s) ~= "none" then
    lines[#lines + 1] =
      "Context use is above the pruning threshold: prefer focused reads and searches over large dumps."
  end
  return table.concat(lines, "\n")
end

maki.api.register_prompt_hint({ slot = "tool_usage", content = hint })

local function bytes(n)
  n = n or 0
  if n >= 1048576 then
    return string.format("%.1f MB", n / 1048576)
  elseif n >= 1024 then
    return string.format("%.1f KB", n / 1024)
  end
  return n .. " B"
end

local function status()
  local id = maki.session.current()
  local s = id and S[id]
  local ctx = ""
  if s and s.cs and s.cw and s.cw > 0 then
    ctx = string.format(" | ctx %d%% (%d/%d)", math.floor(s.cs / s.cw * 100), s.cs, s.cw)
  end
  return string.format(
    "dcp %s | calls %d, saved %s (~%d tok) | dup %d, trim %d, err %d%s",
    cfg.enabled and "on" or "off",
    totals.calls,
    bytes(totals.bytes_saved),
    tok(totals.bytes_saved),
    totals.duplicates,
    totals.truncated,
    totals.errors,
    ctx
  )
end

maki.api.register_command({
  name = "/dcp",
  description = "Show dcp context-pruning status and stats",
  handler = function()
    maki.notify(status(), "info", { title = "dcp" })
  end,
})

local function reset_stats()
  totals = zero()
  for _, s in pairs(S) do
    s.stats = zero()
    s.seen = {}
    s.order = {}
  end
  local p = stats_path()
  if p then
    pcall(maki.fs.rm, p)
  end
end

local function stats_lines()
  local L = {}
  local function head(text)
    L[#L + 1] = { { text, "bold" } }
  end
  local function kv(k, v)
    L[#L + 1] = { { string.format("  %-13s", k), "dim" }, { tostring(v) } }
  end

  head("dcp · context pruning  " .. (cfg.enabled and "on" or "off"))
  kv("mode", (cfg.manual_mode and not cfg.automatic_strategies) and "manual" or "automatic")

  head("Totals")
  kv("calls / pruned", totals.calls .. " / " .. totals.pruned)
  kv("saved", string.format("%s (~%d tok)", bytes(totals.bytes_saved), tok(totals.bytes_saved)))
  kv("dup / trim / err", string.format("%d / %d / %d", totals.duplicates, totals.truncated, totals.errors))

  local id = maki.session.current()
  local s = id and S[id]
  if s then
    head("Session")
    kv("turn / sigs", s.turn .. " / " .. #s.order)
    kv(
      "context",
      (s.cs and s.cw and s.cw > 0)
          and string.format("%d/%d (%d%%)", s.cs, s.cw, math.floor(s.cs / s.cw * 100))
        or "unknown"
    )
    kv("saved", string.format("%s (~%d tok)", bytes(s.stats.bytes_saved), tok(s.stats.bytes_saved)))
  end

  head("Config")
  kv("min / max", tostring(cfg.min_context_limit) .. " / " .. tostring(cfg.max_context_limit))
  kv(
    "budgets",
    string.format(
      "%d/%dB · %d/%dB",
      cfg.soft_max_lines,
      cfg.soft_max_bytes,
      cfg.hard_max_lines,
      cfg.hard_max_bytes
    )
  )
  local dedupe = cfg.strategies.deduplication
  local purge = cfg.strategies.purge_errors
  kv(
    "dedupe / purge",
    (dedupe.enabled and dedupe.mode or "off") .. " / " .. (purge.enabled and (purge.keep_lines .. " lines") or "off")
  )
  return L
end

-- The float draws no inner margin, so add one here: two spaces on each side
-- and a blank line top and bottom.
local function padded(lines)
  local out = { { { "" } } }
  for _, line in ipairs(lines) do
    local l = { { "  " } }
    for _, span in ipairs(line) do
      l[#l + 1] = span
    end
    l[#l + 1] = { "  " }
    out[#out + 1] = l
  end
  out[#out + 1] = { { "" } }
  return out
end

local function stats_window_size(lines)
  local size = maki.ui.terminal_size()
  local width = 0
  for _, line in ipairs(lines) do
    local n = 0
    for _, span in ipairs(line) do
      n = n + maki.ui.display_width(span[1])
    end
    width = math.max(width, n)
  end
  local w = math.min(math.max(width + 4, 44), size.cols - 4)
  local h = math.min(math.max(#lines + 2, 12), size.rows - 2)
  return math.max(20, w), math.max(10, h)
end

local function open_stats_window()
  local buf = maki.ui.buf()
  local width, height = stats_window_size(padded(stats_lines()))
  local rows = math.max(3, height - 2)
  local offset, total = 0, 0
  local function render()
    local all = padded(stats_lines())
    total = #all
    if total > rows then
      offset = math.min(offset, total - rows)
    else
      offset = 0
    end
    local view = {}
    for i = offset + 1, math.min(total, offset + rows) do
      view[#view + 1] = all[i]
    end
    if total > rows then
      view[#view + 1] = { { string.format("  %d/%d  j/k scroll", offset + 1, total), "dim" } }
    end
    buf:set_lines(view)
  end
  render()
  local win = maki.ui.open_win(buf, {
    title = " dcp ",
    width = width,
    height = height,
    border = "rounded",
    footer = { { "j/k", "scroll" }, { "r", "reset" }, { "t", "toggle" }, { "q", "close" } },
  })
  while win:is_open() do
    local ev = win:recv()
    if not ev or ev.type == "close" then
      break
    elseif ev.type == "resize" then
      width, height = stats_window_size(padded(stats_lines()))
      rows = math.max(3, height - 2)
      win:set_config({ width = width, height = height })
      render()
    elseif ev.type == "key" then
      local k = ev.key
      if k == "q" or k == "<Esc>" then
        break
      elseif k == "j" or k == "<Down>" then
        if offset + rows < total then
          offset = offset + 1
          render()
        end
      elseif k == "k" or k == "<Up>" then
        if offset > 0 then
          offset = offset - 1
          render()
        end
      elseif k == "<C-d>" or k == "<PageDown>" then
        offset = math.min(offset + rows, math.max(0, total - rows))
        render()
      elseif k == "<C-u>" or k == "<PageUp>" then
        offset = math.max(0, offset - rows)
        render()
      elseif k == "r" then
        reset_stats()
        offset = 0
        render()
      elseif k == "t" then
        cfg.enabled = not cfg.enabled
        render()
      end
    end
  end
  win:close()
end

maki.api.register_command({
  name = "/dcp-stats",
  description = "Open the dcp stats window",
  handler = open_stats_window,
})

maki.api.register_command({
  name = "/dcp-toggle",
  description = "Enable or disable automatic context pruning",
  handler = function()
    cfg.enabled = not cfg.enabled
    maki.notify("dcp " .. (cfg.enabled and "enabled" or "disabled"), "info", { title = "dcp" })
  end,
})

maki.api.register_command({
  name = "/dcp-reset",
  description = "Clear dcp pruning statistics",
  handler = function()
    reset_stats()
    maki.notify("dcp stats cleared", "info", { title = "dcp" })
  end,
})

maki.api.register_command({
  name = "/dcp-compress",
  description = "Run maki's native compaction (dcp-compress alias)",
  handler = function()
    local ok, err = maki.api.run_command("/compact")
    if ok then
      maki.notify("compaction requested", "info", { title = "dcp" })
    else
      maki.notify("could not run /compact: " .. tostring(err), "error", { title = "dcp" })
    end
  end,
})

-- File APIs are async; they yield, so they cannot run at the plugin top level
-- (a /reload loads the plugin from a C-call boundary). Spawn a task instead.
maki.async.spawn(load_stats)
dbg("loaded (" .. (cfg.enabled and "on" or "off") .. ")")

return M
