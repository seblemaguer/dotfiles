--[[

   Awesome WM configuration template
   github.com/lcpz

--]]

-- {{{ Required libraries
local awesome, client, mouse, screen, tag = awesome, client, mouse, screen, tag
local ipairs, string, os, table, tostring, tonumber, type = ipairs, string, os, table, tostring, tonumber, type

local gears         = require("gears")
local awful         = require("awful")
require("awful.autofocus")
local hotkeys_popup = require("awful.hotkeys_popup").widget
local wibox         = require("wibox")
local beautiful     = require("beautiful")
local naughty       = require("naughty")
local lain          = require("lain")
local pulse         = require("pulseaudio-awesome/pulseaudio")
-- local menubar       = require("menubar")
local freedesktop   = require("freedesktop")

local net_widgets   = require("net_widgets")
-- local switcher      = require("awesome-switcher")

-- }}}

-- {{{ Error handling
if awesome.startup_errors then
   naughty.notify({ preset = naughty.config.presets.critical,
                    title = "Oops, there were errors during startup!",
                    text = awesome.startup_errors })
end

do
   local in_error = false
   awesome.connect_signal("debug::error", function (err)
                             if in_error then return end
                             in_error = true

                             naughty.notify({ preset = naughty.config.presets.critical,
                                              title = "Oops, an error happened!",
                                              text = tostring(err) })
                             in_error = false
   end)
end
-- }}}

-- {{{ Autostart windowless processes
local function run_once(cmd_arr)
   for _, cmd in ipairs(cmd_arr) do
      findme = cmd
      firstspace = cmd:find(" ")
      if firstspace then
         findme = cmd:sub(0, firstspace-1)
      end
      awful.spawn.with_shell(string.format("pgrep -u $USER -x %s > /dev/null || (%s)", findme, cmd))
   end
end

run_once({ "unclutter -root" }) -- entries must be comma-separated
-- }}}

-- {{{ Variable definitions
local chosen_theme = "powerarrow-dark"
local modkey       = "Mod4"
local altkey       = "Mod1"
local terminal     = "terminator"
local editor       = os.getenv("EDITOR")
local browser      = "firefox"

awful.util.terminal = terminal
awful.util.tagnames = { "Original" }
awful.layout.layouts = {
   awful.layout.suit.tile,
   awful.layout.suit.tile.left,
   awful.layout.suit.tile.bottom,
   awful.layout.suit.tile.top,
   awful.layout.suit.floating,
   --awful.layout.suit.fair,
   --awful.layout.suit.fair.horizontal,
   --awful.layout.suit.spiral,
   --awful.layout.suit.spiral.dwindle,
   --awful.layout.suit.max,
   --awful.layout.suit.max.fullscreen,
   --awful.layout.suit.magnifier,
   --awful.layout.suit.corner.nw,
   --awful.layout.suit.corner.ne,
   --awful.layout.suit.corner.sw,
   --awful.layout.suit.corner.se,
   --lain.layout.cascade,
   --lain.layout.cascade.tile,
   --lain.layout.centerwork,
   --lain.layout.centerwork.horizontal,
   --lain.layout.termfair,
   --lain.layout.termfair.center,
}
awful.util.taglist_buttons = awful.util.table.join(
   awful.button({ }, 1, function(t) t:view_only() end),
   awful.button({ modkey }, 1, function(t)
         if client.focus then
            client.focus:move_to_tag(t)
         end
   end),
   awful.button({ }, 3, awful.tag.viewtoggle),
   awful.button({ modkey }, 3, function(t)
         if client.focus then
            client.focus:toggle_tag(t)
         end
   end),
   awful.button({ }, 4, function(t) awful.tag.viewnext(t.screen) end),
   awful.button({ }, 5, function(t) awful.tag.viewprev(t.screen) end)
)

awful.util.tasklist_buttons = awful.util.table.join(
   awful.button({ }, 1, function (c)
         if c == client.focus then
            c.minimized = true
         else
            -- Without this, the following
            -- :isvisible() makes no sense
            c.minimized = false
            if not c:isvisible() and c.first_tag then
               c.first_tag:view_only()
            end
            -- This will also un-minimize
            -- the client, if needed
            client.focus = c
            c:raise()
         end
   end),
   awful.button({ }, 3, function()
         local instance = nil

         return function ()
            if instance and instance.wibox.visible then
               instance:hide()
               instance = nil
            else
               instance = awful.menu.clients({ theme = { width = 250 } })
            end
         end
   end),
   awful.button({ }, 4, function ()
         awful.client.focus.byidx(1)
   end),
   awful.button({ }, 5, function ()
         awful.client.focus.byidx(-1)
end))

lain.layout.termfair.nmaster           = 3
lain.layout.termfair.ncol              = 1
lain.layout.termfair.center.nmaster    = 3
lain.layout.termfair.center.ncol       = 1
lain.layout.cascade.tile.offset_x      = 2
lain.layout.cascade.tile.offset_y      = 32
lain.layout.cascade.tile.extra_padding = 5
lain.layout.cascade.tile.nmaster       = 5
lain.layout.cascade.tile.ncol          = 2

local theme_path = string.format("%s/.config/awesome/themes/%s/theme.lua", os.getenv("HOME"), chosen_theme)
beautiful.init(theme_path)
-- }}}

-- {{{ Menu
local myawesomemenu = {
   { "hotkeys", function() return false, hotkeys_popup.show_help end },
   { "manual", terminal .. " -e man awesome" },
   { "edit config", string.format("%s -e %s %s", terminal, editor, awesome.conffile) },
   { "restart", awesome.restart },
   { "quit", function() awesome.quit() end }
}
awful.util.mymainmenu = freedesktop.menu.build({
      icon_size = beautiful.menu_height or 16,
      before = {
         { "Awesome", myawesomemenu, beautiful.awesome_icon },
         -- other triads can be put here
      },
      after = {
         { "Open terminal", terminal },
         -- other triads can be put here
      }
})
--menubar.utils.terminal = terminal -- Set the Menubar terminal for applications that require it
-- }}}

-- {{{ Screen
-- Re-set wallpaper when a screen's geometry changes (e.g. different resolution)
screen.connect_signal("property::geometry", function(s)
                         -- Wallpaper
                         if beautiful.wallpaper then
                            local wallpaper = beautiful.wallpaper
                            -- If wallpaper is a function, call it with the screen
                            if type(wallpaper) == "function" then
                               wallpaper = wallpaper(s)
                            end
                            gears.wallpaper.maximized(wallpaper, s, true)
                         end
end)

-- Create a wibox for each screen and add it
awful.screen.connect_for_each_screen(function(s) beautiful.at_screen_connect(s) end)
-- }}}

-- {{{ Mouse bindings
root.buttons(awful.util.table.join(
                awful.button({ }, 3, function () awful.util.mymainmenu:toggle() end),
                awful.button({ }, 4, awful.tag.viewnext),
                awful.button({ }, 5, awful.tag.viewprev)
))
-- }}}

-- {{{ Key bindings
globalkeys = awful.util.table.join(
   -- -- Improved alt tab (FIXME: can't exit so it is not working)
   -- awful.key({ "Mod1",           }, "Tab", function ()  switcher.switch( 1, "Alt_L", "Tab", "ISO_Left_Tab")  end),
   -- awful.key({ "Mod1", "Shift"   }, "Tab", function ()  switcher.switch(-1, "Alt_L", "Tab", "ISO_Left_Tab")  end),

   -- Standard alt tab
   awful.key({ "Mod1",           }, "Tab",
      function ()
         -- awful.client.focus.history.previous()
         awful.client.focus.byidx(-1)
         if client.focus then
            client.focus:raise()
         end
   end),

   awful.key({ "Mod1", "Shift"   }, "Tab",
      function ()
         -- awful.client.focus.history.previous()
         awful.client.focus.byidx(1)
         if client.focus then
            client.focus:raise()
         end
   end),

   -- Hotkeys
   awful.key({ modkey,           }, "h",      hotkeys_popup.show_help,                          {description = "show help", group="awesome"}),

   -- System part
   awful.key({ modkey,           }, "l",      function () awful.spawn("xlock -mode blank") end,
      {description = "lock", group="system"}),
   awful.key({ modkey,           }, "s",      hotkeys_popup.show_help,                          {description = "shutdown", group="system"}),

   -- Layout manipulation
   awful.key({ modkey }, "Left", function () awful.client.swap.byidx(  1)    end,               {description = "swap with next client by index", group = "client"}),
   awful.key({ modkey }, "Right", function () awful.client.swap.byidx( -1)    end,              {description = "swap with previous client by index", group = "client"}),


   -- Tag browsing
   awful.key({ modkey, "Control" }, "Left",   awful.tag.viewprev,                               {description = "view previous", group = "tag"}),
   awful.key({ modkey, "Control" }, "Right",  awful.tag.viewnext,                               {description = "view next", group = "tag"}),
   awful.key({ modkey, "Control" }, "Escape", awful.tag.history.restore,                        {description = "go back", group = "tag"}),


   -- Show/Hide Wibox
   awful.key({ modkey }, "b", function ()
         for s in screen do
            s.mywibox.visible = not s.mywibox.visible
            if s.mybottomwibox then
               s.mybottomwibox.visible = not s.mybottomwibox.visible
            end
         end
                              end,                                                                                     {description = "toggle wibox", group = "awesome"}),


   -- Dynamic tagging
   awful.key({ modkey, "Control" }, "n", function () lain.util.add_tag() end,                   {description = "add new tag", group = "tag"}),
   awful.key({ modkey, "Control", "Shift" }, "r", function () lain.util.rename_tag() end,       {description = "rename tag", group = "tag"}),
   awful.key({ modkey, "Control", "Shift" }, "Left", function () lain.util.move_tag(-1) end,    {description = "move tag to the left", group = "tag"}),
   awful.key({ modkey, "Control", "Shift" }, "Right", function () lain.util.move_tag(1) end,    {description = "move tag to the right", group = "tag"}),
   awful.key({ modkey, "Control" }, "d", function () lain.util.delete_tag() end,                {description = "delete tag", group = "tag"}),

   -- Standard program
   awful.key({ modkey,           }, "i", function () awful.spawn(browser) end,                  {description = "open browser", group = "launcher"}),
   awful.key({ modkey,           }, "e", function () awful.spawn("emacs") end,                  {description = "open emacs", group = "launcher"}),
   awful.key({ modkey,           }, "Return", function () awful.spawn(terminal) end,            {description = "open a terminal", group = "launcher"}),
   awful.key({ modkey, "Control" }, "r", awesome.restart,                                       {description = "reload awesome", group = "awesome"}),
   awful.key({ modkey, "Shift"   }, "q", awesome.quit,                                          {description = "quit awesome", group = "awesome"}),

   -- volume
   awful.key({}, "XF86AudioMute", function() pulseaudio.volumeMute() end,                       {description = "muting/unmuting", group = "hotkeys"}),
   awful.key({}, "XF86AudioLowerVolume", function() pulseaudio.volumeDown()  end,               {description = "vol -10%", group = "hotkeys"}),
   awful.key({}, "XF86AudioRaiseVolume", function() pulseaudio.volumeUp() end,                  {description = "vol +10%", group = "hotkeys"}),

   -- -- Brightness
   -- awful.key({ }, "XF86MonBrightnessUp", function () awful.util.spawn("xbacklight -inc 10") end,
   --           {description = "+10%", group = "hotkeys"}),
   -- awful.key({ }, "XF86MonBrightnessDown", function () awful.util.spawn("xbacklight -dec 10") end,
   --           {description = "-10%", group = "hotkeys"}),

   -- Prompt
   awful.key({ modkey }, "r", function () awful.screen.focused().mypromptbox:run() end,         {description = "run prompt", group = "launcher"}),

   awful.key({ modkey }, "x",
      function ()
         awful.prompt.run {
            prompt       = "Run Lua code: ",
            textbox      = awful.screen.focused().mypromptbox.widget,
            exe_callback = awful.util.eval,
            history_path = awful.util.get_cache_dir() .. "/history_eval"
         }
      end,                                                                               {description = "lua execute prompt", group = "awesome"})


   --]]
)

clientkeys = awful.util.table.join(
   awful.key({ altkey, "Shift"   }, "m",      lain.util.magnify_client,
      {description = "magnify client", group = "client"}),
   awful.key({ modkey,           }, "f",
      function (c)
         c.fullscreen = not c.fullscreen
         c:raise()
      end,
      {description = "toggle fullscreen", group = "client"}),
   awful.key({ modkey, "Shift"   }, "c",      function (c) c:kill()                         end,
      {description = "close", group = "client"}),
   awful.key({ modkey, "Control" }, "space",  awful.client.floating.toggle                     ,
      {description = "toggle floating", group = "client"}),
   awful.key({ modkey, "Control" }, "Return", function (c) c:swap(awful.client.getmaster()) end,
      {description = "move to master", group = "client"}),
   awful.key({ modkey,           }, "o",      function (c) c:move_to_screen()               end,
      {description = "move to screen", group = "client"}),
   awful.key({ modkey,           }, "t",      function (c) c.ontop = not c.ontop            end,
      {description = "toggle keep on top", group = "client"}),
   awful.key({ modkey,           }, "n",
      function (c)
         -- The client currently has the input focus, so it cannot be
         -- minimized, since minimized clients can't have the focus.
         c.minimized = true
      end ,
      {description = "minimize", group = "client"}),
   awful.key({ modkey,           }, "m",
      function (c)
         c.maximized = not c.maximized
         c:raise()
      end ,
      {description = "maximize", group = "client"})
)

-- Bind all key numbers to tags.
-- Be careful: we use keycodes to make it works on any keyboard layout.
-- This should map on the top row of your keyboard, usually 1 to 9.
for i = 1, 9 do
   -- Hack to only show tags 1 and 9 in the shortcut window (mod+s)
   local descr_view, descr_toggle, descr_move, descr_toggle_focus
   if i == 1 or i == 9 then
      descr_view = {description = "view tag #", group = "tag"}
      descr_toggle = {description = "toggle tag #", group = "tag"}
      descr_move = {description = "move focused client to tag #", group = "tag"}
      descr_toggle_focus = {description = "toggle focused client on tag #", group = "tag"}
   end
   globalkeys = awful.util.table.join(globalkeys,
                                      -- View tag only.
                                      awful.key({ modkey }, "#" .. i + 9,
                                         function ()
                                            local screen = awful.screen.focused()
                                            local tag = screen.tags[i]
                                            if tag then
                                               tag:view_only()
                                            end
                                         end,
                                         descr_view),
                                      -- Toggle tag display.
                                      awful.key({ modkey, "Control" }, "#" .. i + 9,
                                         function ()
                                            local screen = awful.screen.focused()
                                            local tag = screen.tags[i]
                                            if tag then
                                               awful.tag.viewtoggle(tag)
                                            end
                                         end,
                                         descr_toggle),
                                      -- Move client to tag.
                                      awful.key({ modkey, "Shift" }, "#" .. i + 9,
                                         function ()
                                            if client.focus then
                                               local tag = client.focus.screen.tags[i]
                                               if tag then
                                                  client.focus:move_to_tag(tag)
                                               end
                                            end
                                         end,
                                         descr_move),
                                      -- Toggle tag on focused client.
                                      awful.key({ modkey, "Control", "Shift" }, "#" .. i + 9,
                                         function ()
                                            if client.focus then
                                               local tag = client.focus.screen.tags[i]
                                               if tag then
                                                  client.focus:toggle_tag(tag)
                                               end
                                            end
                                         end,
                                         descr_toggle_focus)
   )
end

clientbuttons = awful.util.table.join(
   awful.button({ }, 1, function (c) client.focus = c; c:raise() end),
   awful.button({ modkey }, 1, awful.mouse.client.move),
   awful.button({ modkey }, 3, awful.mouse.client.resize))

-- Set keys
root.keys(globalkeys)
-- }}}

-- {{{ Rules
-- Rules to apply to new clients (through the "manage" signal).
awful.rules.rules = {
   -- All clients will match this rule.
   { rule = { },
     properties = { border_width = beautiful.border_width,
                    border_color = beautiful.border_normal,
                    focus = awful.client.focus.filter,
                    raise = true,
                    keys = clientkeys,
                    buttons = clientbuttons,
                    screen = awful.screen.preferred,
                    placement = awful.placement.no_overlap+awful.placement.no_offscreen,
                    size_hints_honor = false
     }
   },

   -- Titlebars
   { rule_any = { type = { "dialog", "normal" } },
     properties = { titlebars_enabled = true } },

   -- Set Firefox to always map on the first tag on screen 1.
   { rule = { class = "Firefox" },
     properties = { screen = 1, tag = awful.util.tagnames[1] } },

   { rule = { class = "Gimp", role = "gimp-image-window" },
     properties = { maximized = true } },
}
-- }}}

-- {{{ Signals
-- Signal function to execute when a new client appears.
client.connect_signal("manage", function (c)
                         -- Set the windows at the slave,
                         -- i.e. put it at the end of others instead of setting it master.
                         -- if not awesome.startup then awful.client.setslave(c) end

                         if awesome.startup and
                            not c.size_hints.user_position
                         and not c.size_hints.program_position then
                            -- Prevent clients from being unreachable after screen count changes.
                            awful.placement.no_offscreen(c)
                         end
end)

-- Add a titlebar if titlebars_enabled is set to true in the rules.
client.connect_signal("request::titlebars", function(c)
                         -- Custom
                         if beautiful.titlebar_fun then
                            beautiful.titlebar_fun(c)
                            return
                         end

                         -- Default
                         -- buttons for the titlebar
                         local buttons = awful.util.table.join(
                            awful.button({ }, 1, function()
                                  client.focus = c
                                  c:raise()
                                  awful.mouse.client.move(c)
                            end),
                            awful.button({ }, 3, function()
                                  client.focus = c
                                  c:raise()
                                  awful.mouse.client.resize(c)
                            end)
                         )

                         awful.titlebar(c, {size = 16}) : setup {
                            { -- Left
                               awful.titlebar.widget.iconwidget(c),
                               buttons = buttons,
                               layout  = wibox.layout.fixed.horizontal
                            },
                            { -- Middle
                               { -- Title
                                  align  = "center",
                                  widget = awful.titlebar.widget.titlewidget(c)
                               },
                               buttons = buttons,
                               layout  = wibox.layout.flex.horizontal
                            },
                            { -- Right
                               awful.titlebar.widget.floatingbutton (c),
                               awful.titlebar.widget.maximizedbutton(c),
                               awful.titlebar.widget.stickybutton   (c),
                               awful.titlebar.widget.ontopbutton    (c),
                               awful.titlebar.widget.closebutton    (c),
                               layout = wibox.layout.fixed.horizontal()
                            },
                            layout = wibox.layout.align.horizontal
                                                                }
end)

-- Enable sloppy focus, so that focus follows mouse.
client.connect_signal("mouse::enter", function(c)
                         if awful.layout.get(c.screen) ~= awful.layout.suit.magnifier
                         and awful.client.focus.filter(c) then
                            client.focus = c
                         end
end)

-- No border for maximized clients
client.connect_signal("focus",
                      function(c)
                         if c.maximized then -- no borders if only 1 client visible
                            c.border_width = 0
                         elseif #awful.screen.focused().clients > 1 then
                            c.border_width = beautiful.border_width
                            c.border_color = beautiful.border_focus
                         end
end)
client.connect_signal("unfocus", function(c) c.border_color = beautiful.border_normal end)
-- }}}


-- Autostart app
autorun = true
autorunApps =
   {
      "dropbox start",
      "parcellite",
      "blueman-applet"
   }
if autorun then
   for app = 1, #autorunApps do
      awful.util.spawn(autorunApps[app])
   end
end
-- }}}
