{ config, pkgs, ... }:

let
  user = "madonna";
  myPackages = with pkgs; [
    # Main apps
    emacs29-pgtk
    firefox
    ghostty
    nvim
    tmux

    # i3 utilities
    flameshot
    picom
    networkmanagerapplet
    pavucontrol

    # Dev tools
    ripgrep
    fd
    gcc
    clang
    clang-tools
    gopls
    rust-analyzer
    haskell-language-server
    python3Packages.python-lsp-server
    go
    cargo
    rustc
    ghc
    cabal-install
    python3
    cmake
    libtool

    # Basic tools
    git
    wget
    vim
    htop
    neofetch
    jq
  ];
in
{
  imports = [
    ./hardware-configuration.nix
  ];

  # Bootloader
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.loader.efi.efiSysMountPoint = "/boot";
  boot.loader.timeout = 5;

  # Networking
  networking.hostName = "nixos";
  networking.networkmanager.enable = true;

  # Timezone & locale
  time.timeZone = "Europe/Berlin";
  i18n.defaultLocale = "en_US.UTF-8";

  # X11 / i3
  services.xserver = {
    enable = true;

    windowManager.i3 = {
      enable = true;
      extraPackages = with pkgs; [ dmenu i3status i3lock ];
    };

    displayManager = {
      lightdm.enable = true;
      defaultSession = "none+i3";
    };

    xkb.layout = "de,ru,us";
    xkb.options = "grp:alt_shift_toggle,caps:escape";
  };

  fonts.packages = with pkgs; [
    iosevka
    noto-fonts
    noto-fonts-cjk
  ];

  # Audio
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
    support32Bit = true;
  };

  # Bluetooth
  hardware.bluetooth.enable = true;
  services.blueman.enable = true;
  services.blueman.tray.enable = true;

  # System packages
  environment.systemPackages = myPackages;

  # Shell configuration
  programs.zsh = {
    enable = true;
    ohMyZsh = {
      enable = true;
      theme = "powerlevel10k/powerlevel10k";
      plugins = [ "git" ];
      customPkgs = with pkgs; [
        zsh-powerlevel10k
        zsh-completions
      ];
      updateAutomatically = true;
    };
    syntaxHighlighting.enable = true;
    autosuggestions.enable = true;
  };

  # User account
  users.users.${user} = {
    isNormalUser = true;
    shell = pkgs.zsh;
    extraGroups = [ "wheel" "networkmanager" "audio" ];
    initialPassword = "changeme"; # replace with secure password
  };

  # Enable sudo
  security.sudo.enable = true;

  # System maintenance
  system.autoUpgrade.enable = true;
  nix.gc.automatic = true;
  nix.gc.dates = "weekly";

  # System version
  system.stateVersion = "24.05";
}
