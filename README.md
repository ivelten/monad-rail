# haskell-devcontainer-template

A devcontainer template for Haskell development.

## What's included

### Haskell Toolchain

| Tool | Version |
| --- | --- |
| GHC | 9.10.3 |
| Cabal | 3.12.1.0 |
| Stack | latest |
| GHCup | latest |

### Developer Tools

- **[HLS](https://github.com/haskell/haskell-language-server)** — Haskell Language Server for IDE features (completions, type hints, go-to-definition)
- **[Hoogle](https://hoogle.haskell.org/)** — Local Haskell API search database, pre-generated at build time
- **[Ormolu](https://github.com/tweag/ormolu)** — Opinionated, deterministic code formatter
- **[fast-tags](https://github.com/elaforge/fast-tags)** — Fast tag file generator for Haskell source
- **[cabal-gild](https://github.com/tfausak/cabal-gild)** — Formatter and linter for `.cabal` files
- **[direnv](https://direnv.net/)** — Per-directory environment variable loading, hooked into both `bash` and `zsh`

**Shell:** Zsh (default)

**System tools:** `build-essential`, `curl`, `git`, `pkg-config`, `libffi-dev`, `libgmp-dev`, `libssl-dev`, `zlib1g-dev`, `direnv`, `socat`, `procps`

**VS Code extensions installed automatically:**

- [Haskell](https://marketplace.visualstudio.com/items?itemName=haskell.haskell) — HLS integration
- [Haskell Syntax Highlighting](https://marketplace.visualstudio.com/items?itemName=justusadam.language-haskell)
- [GHCi](https://marketplace.visualstudio.com/items?itemName=eriksik2.vscode-ghci)
- [direnv](https://marketplace.visualstudio.com/items?itemName=mkhl.direnv)
- [Error Lens](https://marketplace.visualstudio.com/items?itemName=usernamehw.errorlens)
- [EditorConfig](https://marketplace.visualstudio.com/items?itemName=editorconfig.editorconfig)
- [Markdown All in One](https://marketplace.visualstudio.com/items?itemName=yzhang.markdown-all-in-one)
- [markdownlint](https://marketplace.visualstudio.com/items?itemName=davidanson.vscode-markdownlint)
- [Claude Code for VS Code](https://marketplace.visualstudio.com/items?itemName=anthropic.claude-code)
- [Haskell GHCi Debug Adapter Phoityne](https://marketplace.visualstudio.com/items?itemName=phoityne.phoityne-vscode)

## Prerequisites

- [Docker](https://www.docker.com/products/docker-desktop)
- [VS Code](https://code.visualstudio.com/) with the [Dev Containers extension](https://marketplace.visualstudio.com/items?itemName=ms-vscode-remote.remote-containers)

For terminal-based usage (see below), you also need:

- [Node.js](https://nodejs.org/)
- [Dev Container CLI](https://github.com/devcontainers/cli): `npm install -g @devcontainers/cli`

## Using this template

### With VS Code

1. Click **Use this template** on GitHub to create a new repository from this template.
2. Clone your new repository and open it in VS Code.
3. When prompted, click **Reopen in Container** (or run the command `Dev Containers: Reopen in Container`).
4. The container will start using the pre-built Docker image and run the post-creation setup automatically. This should complete in just a few minutes.

### From the terminal

A helper script [`open-devcontainer.sh`](open-devcontainer.sh) is provided for launching the devcontainer directly from the console using the [Dev Container CLI](https://github.com/devcontainers/cli).

**Start the container and open a shell:**

```bash
./open-devcontainer.sh
```

The script will:

1. Verify that Node.js and the Dev Container CLI are installed.
2. Start the container (`devcontainer up`).
3. Open an interactive shell inside the container (prefers `zsh`, falls back to `bash`).
4. Prompt you to shut down the container when you exit.

**Rebuild the image before starting:**

```bash
./open-devcontainer.sh -r
```

Use the `-r` flag to force a rebuild of the container image before starting. This is useful after modifying the Dockerfile or `devcontainer.json`.

## Project structure

```text
.
├── .devcontainer/
│   ├── devcontainer.json   # Dev container configuration
│   ├── docker-compose.yml  # App services (uses pre-built Docker image)
├── .editorconfig           # Consistent editor formatting rules
└── .gitignore              # Haskell, Cabal, and VS Code ignores
```

## Customising the template

This template uses a pre-built Docker image from [ivelten/haskell-devcontainer](https://hub.docker.com/r/ivelten/haskell-devcontainer) on Docker Hub. The image includes GHC 9.10.3, Cabal 3.12.1.0, and all developer tools listed above.

**For most projects:** The pre-configured environment should work as-is. You can:

- Create a `cabal.project` and `.cabal` package file at the root of the repository after cloning the template.
- Add a `.envrc` file to the project root for per-project environment variables (direnv is pre-installed and hooked into both Bash and Zsh).

**To customize the toolchain:** If you need different versions of GHC, Cabal, or other tools, you can build your own Docker image. Fork the [haskell-devcontainer](https://github.com/ivelten/haskell-devcontainer) repository and modify the Dockerfile, then update `docker-compose.yml` to reference your custom image.
