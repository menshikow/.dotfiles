#!/bin/bash
# run bash setup.sh

set -e

PLATFORM=$(uname -s)

echo "======================================"
echo "Setting up development environment..."
echo "Platform: $PLATFORM"
echo "======================================"

if [[ "$PLATFORM" == "Darwin" ]]; then
    PKG_MGR="brew"
    echo "installing homebrew packages..."
elif [[ "$PLATFORM" == "Linux" ]]; then
    PKG_MGR="apt"
    echo "installing apt packages..."
else
    echo "Unsupported platform: $PLATFORM"
    exit 1
fi

# ============================================================
# System packages
# ============================================================

if [[ "$PKG_MGR" == "brew" ]]; then
    # macOS
    echo "Installing system tools..."
    brew install \
        emacs \
        git \
        ripgrep \
        fd \
        cmake \
        llvm \
        coreutils \
        multimarkdown
    
    echo "installing fonts..."
    brew tap homebrew/cask-fonts
    brew install --cask font-iosevka

elif [[ "$PKG_MGR" == "apt" ]]; then
    # Ubuntu
    echo "installing system tools..."
    sudo apt update
    sudo apt install -y \
        emacs \
        git \
        ripgrep \
        fd-find \
        cmake \
        build-essential \
        clang \
        clangd \
        lldb \
        gdb \
        multimarkdown \
        fonts-firacode
    
    echo "Installing Iosevka font..."
    mkdir -p ~/.local/share/fonts
    cd /tmp
    wget https://github.com/be5invis/Iosevka/releases/download/v31.9.1/PkgTTF-Iosevka-31.9.1.zip
    unzip -o PkgTTF-Iosevka-31.9.1.zip -d iosevka
    cp iosevka/*.ttf ~/.local/share/fonts/
    fc-cache -f -v
    cd -
fi

# ============================================================
# Python & Python LSP
# ============================================================

echo ""
echo "🐍 Setting up Python..."

# Ensure pip is installed
if ! command -v pip &> /dev/null; then
    if [[ "$PKG_MGR" == "brew" ]]; then
        brew install python
    else
        sudo apt install -y python3-pip python3-venv
    fi
fi

# Install Python language server and formatters
pip install --user --upgrade \
    python-lsp-server \
    pylsp-mypy \
    python-lsp-black \
    pyls-isort \
    flake8 \
    black \
    isort

# ============================================================
# Node.js, npm & JavaScript/TypeScript LSP
# ============================================================

echo ""
echo "📦 Setting up Node.js..."

if ! command -v node &> /dev/null; then
    if [[ "$PKG_MGR" == "brew" ]]; then
        brew install node
    else
        # Install Node.js via NodeSource
        curl -fsSL https://deb.nodesource.com/setup_lts.x | sudo -E bash -
        sudo apt install -y nodejs
    fi
fi

# Install JavaScript/TypeScript language servers and formatters
npm install -g \
    typescript \
    typescript-language-server \
    prettier \
    eslint \
    vscode-langservers-extracted

# ============================================================
# Go & Go LSP
# ============================================================

echo ""
echo "🐹 Setting up Go..."

if ! command -v go &> /dev/null; then
    if [[ "$PKG_MGR" == "brew" ]]; then
        brew install go
    else
        # Install Go from official source
        GO_VERSION="1.23.4"
        wget "https://go.dev/dl/go${GO_VERSION}.linux-amd64.tar.gz"
        sudo rm -rf /usr/local/go
        sudo tar -C /usr/local -xzf "go${GO_VERSION}.linux-amd64.tar.gz"
        rm "go${GO_VERSION}.linux-amd64.tar.gz"
        
        # Add to PATH if not already there
        if ! grep -q "/usr/local/go/bin" ~/.bashrc; then
            echo 'export PATH=$PATH:/usr/local/go/bin:$HOME/go/bin' >> ~/.bashrc
        fi
        export PATH=$PATH:/usr/local/go/bin:$HOME/go/bin
    fi
fi

# Install Go language server and tools
go install golang.org/x/tools/gopls@latest
go install mvdan.cc/gofumpt@latest
go install github.com/segmentio/golines@latest

# ============================================================
# Rust & Rust LSP
# ============================================================

echo ""
echo "🦀 Setting up Rust..."

if ! command -v rustc &> /dev/null; then
    curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y
    source "$HOME/.cargo/env"
fi

# Install rust-analyzer
rustup component add rust-analyzer rustfmt clippy

# ============================================================
# Haskell & Haskell LSP
# ============================================================

echo ""
echo "λ Setting up Haskell..."

if ! command -v ghc &> /dev/null; then
    if [[ "$PKG_MGR" == "brew" ]]; then
        brew install ghc cabal-install
    else
        sudo apt install -y ghc cabal-install
    fi
fi

# Install Haskell Language Server
if ! command -v haskell-language-server-wrapper &> /dev/null; then
    if [[ "$PKG_MGR" == "brew" ]]; then
        brew install haskell-language-server
    else
        cabal update
        cabal install haskell-language-server
    fi
fi

# ============================================================
# Zig & Zig LSP
# ============================================================

echo ""
echo "⚡ Setting up Zig..."

if ! command -v zig &> /dev/null; then
    if [[ "$PKG_MGR" == "brew" ]]; then
        brew install zig
    else
        # Install Zig from official source
        ZIG_VERSION="0.13.0"
        wget "https://ziglang.org/download/${ZIG_VERSION}/zig-linux-x86_64-${ZIG_VERSION}.tar.xz"
        sudo tar -C /usr/local -xf "zig-linux-x86_64-${ZIG_VERSION}.tar.xz"
        sudo ln -sf "/usr/local/zig-linux-x86_64-${ZIG_VERSION}/zig" /usr/local/bin/zig
        rm "zig-linux-x86_64-${ZIG_VERSION}.tar.xz"
    fi
fi

# Install ZLS (Zig Language Server)
if ! command -v zls &> /dev/null; then
    if [[ "$PKG_MGR" == "brew" ]]; then
        brew install zls
    else
        cd /tmp
        git clone https://github.com/zigtools/zls.git --depth 1
        cd zls
        zig build -Doptimize=ReleaseSafe
        sudo cp zig-out/bin/zls /usr/local/bin/
        cd -
    fi
fi

# ============================================================
# Elixir & Elixir LSP
# ============================================================

echo ""
echo "💧 Setting up Elixir..."

if ! command -v elixir &> /dev/null; then
    if [[ "$PKG_MGR" == "brew" ]]; then
        brew install elixir
    else
        sudo apt install -y erlang elixir
    fi
fi

# Install Elixir Language Server
if [[ ! -d ~/.local/share/elixir-ls ]]; then
    cd /tmp
    git clone https://github.com/elixir-lsp/elixir-ls.git --depth 1
    cd elixir-ls
    mix deps.get
    MIX_ENV=prod mix compile
    MIX_ENV=prod mix elixir_ls.release
    mkdir -p ~/.local/share/elixir-ls
    cp -r release/* ~/.local/share/elixir-ls/
    chmod +x ~/.local/share/elixir-ls/language_server.sh
    sudo ln -sf ~/.local/share/elixir-ls/language_server.sh /usr/local/bin/elixir-ls
    cd -
fi

# ============================================================
# Additional tools
# ============================================================

echo ""
echo "🔧 Installing additional tools..."

if [[ "$PKG_MGR" == "brew" ]]; then
    brew install \
        docker \
        docker-compose \
        terraform \
        ansible
else
    # Docker
    if ! command -v docker &> /dev/null; then
        curl -fsSL https://get.docker.com -o get-docker.sh
        sudo sh get-docker.sh
        sudo usermod -aG docker $USER
        rm get-docker.sh
    fi
    
    # Docker Compose
    if ! command -v docker-compose &> /dev/null; then
        sudo curl -L "https://github.com/docker/compose/releases/latest/download/docker-compose-$(uname -s)-$(uname -m)" -o /usr/local/bin/docker-compose
        sudo chmod +x /usr/local/bin/docker-compose
    fi
    
    # Terraform
    if ! command -v terraform &> /dev/null; then
        wget -O- https://apt.releases.hashicorp.com/gpg | sudo gpg --dearmor -o /usr/share/keyrings/hashicorp-archive-keyring.gpg
        echo "deb [signed-by=/usr/share/keyrings/hashicorp-archive-keyring.gpg] https://apt.releases.hashicorp.com $(lsb_release -cs) main" | sudo tee /etc/apt/sources.list.d/hashicorp.list
        sudo apt update && sudo apt install -y terraform
    fi
    
    # Ansible
    sudo apt install -y ansible
fi

# ============================================================
# Emacs setup
# ============================================================

echo ""
echo "📝 Setting up Emacs configuration..."

# Backup existing config if it exists
if [[ -f ~/.emacs.d/init.el ]]; then
    echo "Backing up existing init.el to init.el.backup"
    cp ~/.emacs.d/init.el ~/.emacs.d/init.el.backup
fi

echo "idi nahui"
