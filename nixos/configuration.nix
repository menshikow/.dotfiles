{ config, pkgs, ... }:

{
  imports =
    [ 
      ./hardware-configuration.nix
    ];

  # Bootloader
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  # Networking
  networking.hostName = "nixos";
  networking.networkmanager.enable = true;

  # Time zone and locale
  time.timeZone = "Europe/Berlin";
  i18n.defaultLocale = "en_US.UTF-8";

  # X11 and i3
  services.xserver = {
    enable = true;
    
    # i3 window manager
    windowManager.i3 = {
      enable = true;
      extraPackages = with pkgs; [
        dmenu
        i3status
        i3lock
      ];
    };
    
    # Display manager
    displayManager = {
      lightdm.enable = true;
      defaultSession = "none+i3";
    };
    
    # Keyboard layouts (matching your i3 config)
    xkb.layout = "de,ru,us";
    xkb.options = "grp:alt_shift_toggle,caps:escape";
  };

  # Fonts
  fonts.packages = with pkgs; [
    iosevka
  ];

  # Audio
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
  };

  # Bluetooth
  hardware.bluetooth.enable = true;
  services.blueman.enable = true;

  # Essential packages
  environment.systemPackages = with pkgs; [
    # Main apps
    emacs29-pgtk  # GTK build with better Wayland support
    firefox
    ghostty
    nvim 
    
    # i3 utilities
    flameshot
    picom
    networkmanagerapplet
    pavucontrol
    
    # Emacs dependencies
    ripgrep       # for consult-ripgrep
    fd            # for consult-find
    
    # C/C++ development
    gcc
    clang
    clang-tools   # includes clangd LSP
    
    # Language servers for eglot
    gopls         # Go
    rust-analyzer # Rust
    haskell-language-server # Haskell
    python3Packages.python-lsp-server # Python
    
    # Language tooling
    go
    cargo
    rustc
    ghc
    cabal-install
    python3
    
    # Terminal tools
    cmake         # for vterm compilation
    libtool
    
    # Basic tools
    git
    wget
    vim
  ];

  # Shell configuration
  programs.zsh = {
    enable = true;
    ohMyZsh = {
      enable = true;
      theme = "powerlevel10k/powerlevel10k";
      plugins = [ "git" ];
      customPkgs = with pkgs; [
        zsh-powerlevel10k
      ];
    };
    syntaxHighlighting.enable = true;
    autosuggestions.enable = true;
  };

  # User account
  users.users.youruser = {
    isNormalUser = true;
    shell = pkgs.zsh;
    extraGroups = [ "wheel" "networkmanager" ];
    initialPassword = "changeme";
  };

  # Enable sudo
  security.sudo.enable = true;

  # System version
  system.stateVersion = "24.05";
}
