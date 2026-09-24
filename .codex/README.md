# Codex setup

`.codex/config.toml` pins the Haskell marketplace and its Hoogle dependency to Git
revisions and enables their plugins. External skills and scripts stay in Codex's
plugin cache.

## First use

Trust the checkout so Codex reads its project configuration. On a new installation,
run the native refresh command from the repository root to fetch the declared sources:

```sh
codex plugin marketplace upgrade
```

Restart Codex to load the plugins. The project configuration enables all six;
there is no need to add these marketplaces to personal configuration. Verified
with Codex CLI 0.156.1 and an initially empty plugin cache.

## Prerequisites

Install GHC/Cabal, HLS, Fourmolu, cabal-gild, Hoogle, Bash 5, jq, and curl on `PATH`.
Hoogle needs a database; use its plugin's initialization instructions. Hooks and
LSP tools depend on client support. Format explicitly and use Cabal diagnostics
when those integrations are unavailable.

After updating the pinned revisions in `.codex/config.toml`, refresh the
marketplaces and restart Codex.

References: [Codex marketplace configuration](https://learn.chatgpt.com/docs/config-file/config-reference)
and [native marketplace commands](https://developers.openai.com/plugins/build/plugins#add-a-marketplace-source).
