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
  compress_tool = true,
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
local user_opts

function M.setup(opts)
  user_opts = opts
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

local function user_limit(s)
  return s
    and s.user_limit_ratio
    and s.cs
    and s.cw
    and s.cw > 0
    and s.cs / s.cw >= s.user_limit_ratio
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

local function config_path()
  local ok, dir = pcall(maki.env.state_dir)
  return ok and dir and maki.fs.joinpath(dir, "dcp", "config.json") or nil
end

local function load_config()
  local p = config_path()
  if not p or not maki.fs.metadata(p) then
    return
  end
  local text = maki.fs.read(p)
  local ok, v = pcall(maki.json.decode, text or "")
  if ok and type(v) == "table" then
    merge(cfg, v)
    -- Saved UI edits load after require() returns, so re-apply init.lua opts
    -- last to keep them authoritative.
    if user_opts then
      merge(cfg, user_opts)
    end
  end
end

local function save_config()
  local p = config_path()
  if not p then
    return
  end
  local ok, encoded = pcall(maki.json.encode, cfg)
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
    or protected(ctx)
  then
    return prev(out, ctx)
  end
  local s = st(ctx.session_id)
  local forced = s.force
  if not forced and cfg.manual_mode and not cfg.automatic_strategies then
    return prev(out, ctx)
  end
  s.stats.calls = s.stats.calls + 1
  totals.calls = totals.calls + 1
  if not dedupe(s, out, ctx) then
    local level = forced or pressure(s)
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
    if ev.event == "AutoCompacting" and d.context_size and d.context_window and d.context_window > 0 then
      s.user_limit_ratio = d.context_size / d.context_window
    end
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
  if s then
    pressure(s)
  end
  local hit = user_limit(s)
  if hit then
    lines[#lines + 1] =
      "User limit has been triggered, please compress asap."
  end
  if cfg.compress_tool and (hit or (cfg.manual_mode and not cfg.automatic_strategies)) then
    lines[#lines + 1] =
      "If context is still too large for what you need to read, call the compress tool: it forces aggressive pruning of later tool results in this session (already-sent content is unaffected)."
  end
  return table.concat(lines, "\n")
end

maki.api.register_prompt_hint({ slot = "tool_usage", content = hint })

maki.api.register_tool({
  name = "compress",
  kind = "context",
  description = [[Force dcp to prune this session's tool output.

Use it when context use is high but you still need to read more: later tool
results in this session are then pruned at the chosen level immediately,
instead of waiting for the automatic context threshold. It does not shrink
content that is already in the context.

- "hard" (default): keep only the hard_max_lines / hard_max_bytes of each result.
- "soft": keep the softer soft_max_lines / soft_max_bytes limits.
- "auto": clear the override and return to threshold-driven pruning.]],
  schema = {
    type = "object",
    properties = {
      level = {
        type = "string",
        enum = { "hard", "soft", "auto" },
        description = "Pruning level for later tool results in this session. Default: hard.",
      },
    },
  },
  handler = function(input, ctx)
    if not cfg.compress_tool then
      return { llm_output = "error: the compress tool is disabled in dcp settings", is_error = true }
    end
    local level = (input and input.level) or "hard"
    if level ~= "hard" and level ~= "soft" and level ~= "auto" then
      return { llm_output = "error: level must be hard, soft or auto", is_error = true }
    end
    local sid = ctx and (ctx.session_id or ctx.session)
    if type(sid) ~= "string" then
      sid = maki.session.current()
    end
    if type(sid) ~= "string" then
      return { llm_output = "error: no active session", is_error = true }
    end
    local s = st(sid)
    s.force = level == "auto" and nil or level
    local out = { "dcp: pruning for later tool results is now " .. (s.force or "auto (threshold)") .. "." }
    if s.cs and s.cw then
      out[#out + 1] = string.format("Context: %d of %d tokens (pressure: %s).", s.cs, s.cw, pressure(s))
    elseif s.cs then
      out[#out + 1] = string.format("Context: %d tokens.", s.cs)
    end
    return { llm_output = table.concat(out, "\n") }
  end,
})

local function bytes(n)
  n = n or 0
  if n >= 1048576 then
    return string.format("%.1f MB", n / 1048576)
  elseif n >= 1024 then
    return string.format("%.1f KB", n / 1024)
  end
  return n .. " B"
end

local function count(n)
  if type(n) ~= "number" then
    return tostring(n)
  end
  if n >= 1e6 then
    return string.format("%.1fM", n / 1e6)
  elseif n >= 1000 then
    local v = n / 1000
    return v % 1 == 0 and string.format("%dK", v) or string.format("%.1fK", v)
  end
  return tostring(n)
end

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
  local items, kw = {}, 0
  local function head(text)
    items[#items + 1] = { text }
  end
  local function kv(k, v)
    kw = math.max(kw, #k)
    items[#items + 1] = { k, tostring(v) }
  end

  head("Dynamic Context Pruning (DCP)")
  kv("enabled", cfg.enabled and "on" or "off")
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
    kv("pruning", s.force and (s.force .. " (forced)") or "threshold")
    kv(
      "context",
      (s.cs and s.cw and s.cw > 0)
          and string.format("%s/%s (%d%%)", count(s.cs), count(s.cw), math.floor(s.cs / s.cw * 100))
        or "unknown"
    )
    kv("saved", string.format("%s (~%d tok)", bytes(s.stats.bytes_saved), tok(s.stats.bytes_saved)))
  end

  head("Config")
  kv("token limits", count(cfg.min_context_limit) .. " / " .. count(cfg.max_context_limit))
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
  local fmt = "  %-" .. (kw + 2) .. "s"
  local L = {}
  for _, item in ipairs(items) do
    if item[2] == nil then
      L[#L + 1] = { { item[1], "bold" } }
    else
      L[#L + 1] = { { string.format(fmt, item[1]), "dim" }, { item[2] } }
    end
  end
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

local FIELDS = {
  { "enabled", { "enabled" }, "bool" },
  { "debug", { "debug" }, "bool" },
  { "allow subagents", { "allow_subagents" }, "bool" },
  { "manual mode", { "manual_mode" }, "bool" },
  { "auto strategies", { "automatic_strategies" }, "bool" },
  { "min context", { "min_context_limit" }, "limit" },
  { "max context", { "max_context_limit" }, "limit" },
  { "soft max lines", { "soft_max_lines" }, "int", 1 },
  { "soft max bytes", { "soft_max_bytes" }, "int", 1 },
  { "hard max lines", { "hard_max_lines" }, "int", 1 },
  { "hard max bytes", { "hard_max_bytes" }, "int", 1 },
  { "always max bytes", { "always_max_bytes" }, "int", 0 },
  { "dedupe enabled", { "strategies", "deduplication", "enabled" }, "bool" },
  { "dedupe mode", { "strategies", "deduplication", "mode" }, "enum", { "identical", "input" } },
  { "purge enabled", { "strategies", "purge_errors", "enabled" }, "bool" },
  { "purge keep lines", { "strategies", "purge_errors", "keep_lines" }, "int", 1 },
  { "purge min bytes", { "strategies", "purge_errors", "min_bytes" }, "int", 0 },
  { "prompt hint", { "prompt_hint" }, "bool" },
  { "compress tool", { "compress_tool" }, "bool" },
  { "persist stats", { "persist_stats" }, "bool" },
  { "max seen", { "max_seen" }, "int", 1 },
  { "protected tools", { "protected_tools" }, "list" },
  { "protected files", { "protected_file_patterns" }, "list" },
  { "protected kinds", { "protected_kinds" }, "list" },
  { "dedupe protect", { "strategies", "deduplication", "protected_tools" }, "list" },
  { "purge protect", { "strategies", "purge_errors", "protected_tools" }, "list" },
}

local function field_value(field)
  local v = cfg
  for i = 1, #field[2] - 1 do
    v = v[field[2][i]]
  end
  return v[field[2][#field[2]]]
end

local function set_field(field, value)
  local t = cfg
  for i = 1, #field[2] - 1 do
    t = t[field[2][i]]
  end
  t[field[2][#field[2]]] = value
end

local function field_text(value)
  if type(value) == "table" then
    return #value .. " items"
  elseif type(value) == "boolean" then
    return value and "on" or "off"
  end
  return tostring(value)
end

local function settings_lines(sel)
  local L = { { { "Dynamic Context Pruning (DCP)", "bold" } } }
  local kw = 0
  for _, f in ipairs(FIELDS) do
    kw = math.max(kw, #f[1])
  end
  local fmt = "  %-" .. (kw + 2) .. "s"
  for i, f in ipairs(FIELDS) do
    local marker = i == sel and "> " or "  "
    local style = i == sel and "bold" or "dim"
    L[#L + 1] = { { marker .. string.format(fmt, f[1]), style }, { field_text(field_value(f)) } }
  end
  return L
end

local function cycle_field(field)
  local value = field_value(field)
  if field[3] == "bool" then
    set_field(field, not value)
  elseif field[3] == "enum" then
    local values = field[4]
    local idx = 1
    for i, v in ipairs(values) do
      if v == value then
        idx = i
      end
    end
    set_field(field, values[idx % #values + 1])
  else
    return false
  end
  save_config()
  return true
end

local function prompt_field(label, initial)
  local TextInput = require("maki.text_input")
  local ti = TextInput.new()
  ti:insert_text(initial)
  local buf = maki.ui.buf()
  local size = maki.ui.terminal_size()
  local width = math.min(math.max(#label + #initial + 12, 44), size.cols - 4)
  local function draw()
    buf:set_lines(ti:render(label .. ": ", #label + 2, width - 2).lines)
  end
  draw()
  local win = maki.ui.open_win(buf, {
    title = " dcp edit ",
    width = width,
    height = 5,
    border = "rounded",
    footer = { { "<CR>", "save" }, { "<Esc>", "cancel" } },
  })
  local result
  while win:is_open() do
    local ev = win:recv()
    if not ev or ev.type == "close" then
      break
    elseif ev.type == "key" then
      local k = ev.key
      if k == "<Esc>" then
        break
      elseif k == "<CR>" then
        result = ti:value()
        break
      elseif ti:handle_key(k) ~= TextInput.Result.IGNORED then
        draw()
      end
    end
  end
  win:close()
  return result
end

local function apply_field(field)
  local kind = field[3]
  if kind == "bool" or kind == "enum" then
    return cycle_field(field)
  end
  local current = field_value(field)
  local initial = type(current) == "table" and table.concat(current, ", ") or tostring(current)
  local raw = prompt_field(field[1], initial)
  if not raw then
    return false
  end
  raw = raw:match("^%s*(.-)%s*$")
  if kind == "int" then
    local n = tonumber(raw)
    if not n then
      maki.notify("not a number: " .. raw, "error", { title = "dcp" })
      return false
    end
    n = math.floor(n)
    if field[4] and n < field[4] then
      n = field[4]
    end
    set_field(field, n)
  elseif kind == "limit" then
    if raw:match("^%d+%%$") then
      set_field(field, raw)
    else
      local n = tonumber(raw)
      if not n then
        maki.notify("use a number or NN%", "error", { title = "dcp" })
        return false
      end
      set_field(field, math.floor(n))
    end
  elseif kind == "list" then
    local list = {}
    for item in raw:gmatch("[^,]+") do
      item = item:match("^%s*(.-)%s*$")
      if item ~= "" then
        list[#list + 1] = item
      end
    end
    set_field(field, list)
  else
    set_field(field, raw)
  end
  save_config()
  return true
end

local function open_settings_window()
  local sel = 1
  local buf = maki.ui.buf()
  local function render()
    buf:set_lines(padded(settings_lines(sel)))
  end
  local width, height = stats_window_size(padded(settings_lines(sel)))
  render()
  local win = maki.ui.open_win(buf, {
    title = " dcp config ",
    width = width,
    height = height,
    border = "rounded",
    footer = { { "j/k", "move" }, { "<CR>", "edit" }, { "<Space>", "toggle" }, { "q", "close" } },
  })
  while win:is_open() do
    local ev = win:recv()
    if not ev or ev.type == "close" then
      break
    elseif ev.type == "resize" then
      width, height = stats_window_size(padded(settings_lines(sel)))
      win:set_config({ width = width, height = height })
    elseif ev.type == "key" then
      local k = ev.key
      if k == "q" or k == "<Esc>" then
        break
      elseif k == "j" or k == "<Down>" then
        if sel < #FIELDS then
          sel = sel + 1
          render()
        end
      elseif k == "k" or k == "<Up>" then
        if sel > 1 then
          sel = sel - 1
          render()
        end
      elseif k == "<CR>" then
        if apply_field(FIELDS[sel]) then
          width, height = stats_window_size(padded(settings_lines(sel)))
          win:set_config({ width = width, height = height })
          render()
        end
      elseif k == "<Space>" then
        local f = FIELDS[sel]
        if f[3] == "bool" or f[3] == "enum" then
          cycle_field(f)
          render()
        end
      end
    end
  end
  win:close()
end

local function open_stats_window()
  local buf = maki.ui.buf()
  local function render()
    buf:set_lines(padded(stats_lines()))
  end
  local width, height = stats_window_size(padded(stats_lines()))
  render()
  local win = maki.ui.open_win(buf, {
    title = " dcp ",
    width = width,
    height = height,
    border = "rounded",
    footer = { { "e", "config" }, { "t", "toggle" }, { "q", "close" } },
  })
  local edit = false
  while win:is_open() do
    local ev = win:recv()
    if not ev or ev.type == "close" then
      break
    elseif ev.type == "resize" then
      width, height = stats_window_size(padded(stats_lines()))
      win:set_config({ width = width, height = height })
    elseif ev.type == "key" then
      local k = ev.key
      if k == "q" or k == "<Esc>" then
        break
      elseif k == "e" then
        edit = true
        break
      elseif k == "t" then
        cfg.enabled = not cfg.enabled
        save_config()
        width, height = stats_window_size(padded(stats_lines()))
        win:set_config({ width = width, height = height })
        render()
      end
    end
  end
  win:close()
  if edit then
    open_settings_window()
  end
end

maki.api.register_command({
  name = "/dcp-stats",
  description = "Open the dcp stats window",
  handler = open_stats_window,
})

maki.api.register_command({
  name = "/dcp-config",
  description = "Open the dcp settings window",
  handler = open_settings_window,
})

maki.api.register_command({
  name = "/dcp-reset",
  description = "Clear dcp pruning statistics",
  handler = function()
    reset_stats()
    maki.notify("dcp stats cleared", "info", { title = "dcp" })
  end,
})

-- File APIs are async; they yield, so they cannot run at the plugin top level
-- (a /reload loads the plugin from a C-call boundary). Spawn a task instead.
maki.async.spawn(function()
  load_config()
  load_stats()
end)
dbg("loaded (" .. (cfg.enabled and "on" or "off") .. ")")

return M
