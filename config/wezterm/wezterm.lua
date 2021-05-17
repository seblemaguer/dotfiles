local wezterm = require 'wezterm';
return {
   -- Window
   hide_tab_bar_if_only_one_tab = true,
   enable_scroll_bar = true,
   scrollback_lines = 10000,

   -- Color scheme
   colors = {
      tab_bar = {
         -- The color of the strip that goes along the top of the window
         background = "#262626",

         -- The active tab is the one that has focus in the window
         active_tab = {
            bg_color = "#404040", -- The color of the background area for the tab
            fg_color = "#c0c0c0", -- The color of the text for the tab
            intensity = "Bold",
            underline = "Single",
            italic = false,
            strikethrough = false,
         },

         -- Inactive tabs are the tabs that do not have focus
         inactive_tab = {
            bg_color = "#202020",
            fg_color = "#808080",
         },

         -- You can configure some alternate styling when the mouse pointer
         -- moves over inactive tabs
         inactive_tab_hover = {
            bg_color = "#363636",
            fg_color = "#909090",
            italic = false,
         }
      },

      -- Default
      foreground = "black",
      background = "#ecf0f1",

      -- Selection colors
      selection_fg = "black",
      selection_bg = "#1abc9c",

      -- Cursor
      cursor_bg = "#e67e22",
      cursor_fg = "black",
      cursor_border = "#e67e22",

      -- The color of the scrollbar "thumb"; the portion that represents the current viewport
      scrollbar_thumb = "#909090",

      -- The color of the split lines between panes
      split = "#909090",

      -- Terminal color scheme
   },

   -- Font
   harfbuzz_features = {"kern", "liga", "clig", "calt"},
   font = wezterm.font'Roboto Mono',
   font_rules = {
      {
         intensity = 'Normal',
         font = wezterm.font'Roboto Mono'
      },
      {
         intensity = 'Bold',
         font = wezterm.font'Roboto Mono',
         bold = true
      }
   },
   font_size = 10.0,


   -- Keys
   keys = {
      -- Tabs
      {key="t", mods="CTRL|SHIFT", action=wezterm.action{SpawnTab="CurrentPaneDomain"}},

      -- Splits
      {key="o", mods="CTRL|SHIFT", action=wezterm.action{SplitVertical={domain="CurrentPaneDomain"}}},
      {key="e", mods="CTRL|SHIFT", action=wezterm.action{SplitHorizontal={domain="CurrentPaneDomain"}}},

      -- ???
      {key="w", mods="CTRL|SHIFT", action=wezterm.action{CloseCurrentPane={confirm=false}}},

      -- move tabs
      {key="LeftArrow", mods="CTRL", action=wezterm.action{MoveTabRelative=-1}},
      {key="RightArrow", mods="CTRL", action=wezterm.action{MoveTabRelative=1}},
   },
}
