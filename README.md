Dynamic context pruning ported from an opencode plugin for it ([here](https://github.com/Tarquinen/opencode-dynamic-context-pruning))

Writen by a clanker (deepseek)

```lua
maki.pack.add({
    { src = "https://github.com/Doloro1978/dcp-maki-plugin", version = "master" },
}, {
    load = function(package)
        require("dcp")
    end,
})
```
