return {
  {
    "tjdevries/colorbuddy.nvim",
    lazy = false,
    priority = 1000,
  },
  {
    "tjdevries/gruvbuddy.nvim",
    lazy = false,
    priority = 1000,
    dependencies = { "tjdevries/colorbuddy.nvim" },
  },
}
