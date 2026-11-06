-- Loads the dcp plugin. Copy or symlink this directory to a maki config dir
-- (or <project>/.maki) and adjust the setup call below.

local dcp = require("dcp")

-- Optional. Every key is optional; see lua/dcp.lua for the full default table.
-- dcp.setup({
--   min_context_limit = "20%",
--   max_context_limit = "40%",
--   soft_max_lines = 400,
--   hard_max_lines = 120,
--   protected_file_patterns = { "*.lock", "dist/**" },
--   strategies = {
--     deduplication = { enabled = true, mode = "identical" },
--     purge_errors = { enabled = true, keep_lines = 3 },
--   },
-- })

return dcp
