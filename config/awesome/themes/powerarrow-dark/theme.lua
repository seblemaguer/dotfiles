--[[

   Powerarrow Dark Awesome WM theme
   - original: github.com/lcpz
   - some adaptation (adding some outside widgets)

--]]

-- Default packages
local gears = require("gears")
local lain  = require("lain")
local awful = require("awful")
local wibox = require("wibox")

-- Added packages
local leds_widget = require("leds-widget")
local battery_widget = require("battery-widget")
local calendar = require("calendar")
local cpuinfo = require("cpuinfo")

local os    = { getenv = os.getenv }

local theme                                     = {}
theme.dir                                       = os.getenv("HOME") .. "/.config/awesome/themes/powerarrow-dark"
theme.wallpaper                                 = theme.dir .. "/wall.png"
theme.font                                      = "xos4 Terminus 10"
theme.fg_normal                                 = "#DDDDFF"
theme.fg_focus                                  = "#EA6F81"
theme.fg_urgent                                 = "#CC9393"
theme.bg_normal                                 = "#1A1A1A"
theme.bg_focus                                  = "#313131"
theme.bg_urgent                                 = "#1A1A1A"
theme.border_width                              = 1
theme.border_normal                             = "#3F3F3F"
theme.border_focus                              = "#7F7F7F"
theme.border_marked                             = "#CC9393"
theme.tasklist_bg_focus                         = "#1A1A1A"
theme.titlebar_bg_focus                         = theme.bg_focus
theme.titlebar_bg_normal                        = theme.bg_normal
theme.titlebar_fg_focus                         = theme.fg_focus
theme.menu_height                               = 16
theme.menu_width                                = 140
theme.menu_submenu_icon                         = theme.dir .. "/icons/submenu.png"
theme.taglist_squares_sel                       = theme.dir .. "/icons/square_sel.png"
theme.taglist_squares_unsel                     = theme.dir .. "/icons/square_unsel.png"
theme.layout_tile                               = theme.dir .. "/icons/tile.png"
theme.layout_tileleft                           = theme.dir .. "/icons/tileleft.png"
theme.layout_tilebottom                         = theme.dir .. "/icons/tilebottom.png"
theme.layout_tiletop                            = theme.dir .. "/icons/tiletop.png"
theme.layout_fairv                              = theme.dir .. "/icons/fairv.png"
theme.layout_fairh                              = theme.dir .. "/icons/fairh.png"
theme.layout_spiral                             = theme.dir .. "/icons/spiral.png"
theme.layout_dwindle                            = theme.dir .. "/icons/dwindle.png"
theme.layout_max                                = theme.dir .. "/icons/max.png"
theme.layout_fullscreen                         = theme.dir .. "/icons/fullscreen.png"
theme.layout_magnifier                          = theme.dir .. "/icons/magnifier.png"
theme.layout_floating                           = theme.dir .. "/icons/floating.png"
theme.widget_ac                                 = theme.dir .. "/icons/ac.png"
theme.widget_mem                                = theme.dir .. "/icons/mem.png"
theme.widget_cpu                                = theme.dir .. "/icons/cpu.png"
theme.widget_temp                               = theme.dir .. "/icons/temp.png"
theme.widget_net                                = theme.dir .. "/icons/net.png"
theme.widget_hdd                                = theme.dir .. "/icons/hdd.png"
theme.widget_music                              = theme.dir .. "/icons/note.png"
theme.widget_music_on                           = theme.dir .. "/icons/note_on.png"
theme.widget_vol                                = theme.dir .. "/icons/vol.png"
theme.widget_vol_low                            = theme.dir .. "/icons/vol_low.png"
theme.widget_vol_no                             = theme.dir .. "/icons/vol_no.png"
theme.widget_vol_mute                           = theme.dir .. "/icons/vol_mute.png"
theme.widget_mail                               = theme.dir .. "/icons/mail.png"
theme.widget_mail_on                            = theme.dir .. "/icons/mail_on.png"
theme.tasklist_plain_task_name                  = true
theme.tasklist_disable_icon                     = true
theme.useless_gap                               = 0
theme.titlebar_close_button_focus               = theme.dir .. "/icons/titlebar/close_focus.png"
theme.titlebar_close_button_normal              = theme.dir .. "/icons/titlebar/close_normal.png"
theme.titlebar_ontop_button_focus_active        = theme.dir .. "/icons/titlebar/ontop_focus_active.png"
theme.titlebar_ontop_button_normal_active       = theme.dir .. "/icons/titlebar/ontop_normal_active.png"
theme.titlebar_ontop_button_focus_inactive      = theme.dir .. "/icons/titlebar/ontop_focus_inactive.png"
theme.titlebar_ontop_button_normal_inactive     = theme.dir .. "/icons/titlebar/ontop_normal_inactive.png"
theme.titlebar_sticky_button_focus_active       = theme.dir .. "/icons/titlebar/sticky_focus_active.png"
theme.titlebar_sticky_button_normal_active      = theme.dir .. "/icons/titlebar/sticky_normal_active.png"
theme.titlebar_sticky_button_focus_inactive     = theme.dir .. "/icons/titlebar/sticky_focus_inactive.png"
theme.titlebar_sticky_button_normal_inactive    = theme.dir .. "/icons/titlebar/sticky_normal_inactive.png"
theme.titlebar_floating_button_focus_active     = theme.dir .. "/icons/titlebar/floating_focus_active.png"
theme.titlebar_floating_button_normal_active    = theme.dir .. "/icons/titlebar/floating_normal_active.png"
theme.titlebar_floating_button_focus_inactive   = theme.dir .. "/icons/titlebar/floating_focus_inactive.png"
theme.titlebar_floating_button_normal_inactive  = theme.dir .. "/icons/titlebar/floating_normal_inactive.png"
theme.titlebar_maximized_button_focus_active    = theme.dir .. "/icons/titlebar/maximized_focus_active.png"
theme.titlebar_maximized_button_normal_active   = theme.dir .. "/icons/titlebar/maximized_normal_active.png"
theme.titlebar_maximized_button_focus_inactive  = theme.dir .. "/icons/titlebar/maximized_focus_inactive.png"
theme.titlebar_maximized_button_normal_inactive = theme.dir .. "/icons/titlebar/maximized_normal_inactive.png"

local markup = lain.util.markup
local separators = lain.util.separators

-- Textclock
local clockicon = wibox.widget.imagebox(theme.widget_clock)
local clock = awful.widget.watch(
   "date +'%a %d %b %R'", 60,
   function(widget, stdout)
      widget:set_markup(" " .. markup.font(theme.font, stdout))
   end
)

-- Calendar
theme.cal = calendar({
      fdow = 7,                  -- Set Sunday as first day of the week (default is
      -- 1 = Monday)
})
theme.cal:attach(clock)

-- MEM
local memicon = wibox.widget.imagebox(theme.widget_mem)
local mem = lain.widget.mem({
      settings = function()
         widget:set_markup(markup.font(theme.font, " " .. mem_now.used .. "MB "))
      end
})

-- CPU
local cpuicon = wibox.widget.imagebox(theme.widget_cpu)
local cpu = lain.widget.cpu({
      settings = function()
         widget:set_markup(markup.font(theme.font, " " .. cpu_now.usage .. "% "))
      end
})

local cpu_info = cpuinfo()

-- Coretemp
local tempicon = wibox.widget.imagebox(theme.widget_temp)
local temp = lain.widget.temp({
      settings = function()
         widget:set_markup(markup.font(theme.font, " " .. coretemp_now .. "°C "))
      end
})

-- / fs
local fsicon = wibox.widget.imagebox(theme.widget_hdd)
-- theme.fs = lain.widget.fs({
--     options  = "--exclude-type=tmpfs",
--     notification_preset = { fg = theme.fg_normal, bg = theme.bg_normal, font = "xos4 Terminus 10" },
--     settings = function()
--         widget:set_markup(markup.font(theme.font, " " .. fs_now.used .. "% "))
--     end
-- })

-- Battery
local baticon = wibox.widget.imagebox(theme.widget_battery)
local bat = battery_widget {
   ac = "AC",
   adapter = "BAT0",
   ac_prefix = "AC: ",
   battery_prefix = "Bat: ",
   percent_colors = {
      { 25, "red"   },
      { 50, "orange"},
      {999, "green" },
   },
   listen = true,
   timeout = 10,
   widget_text = "${AC_BAT}${color_on}${percent}%${color_off}",
   widget_font = "xos4 Terminus 10",
   tooltip_text = "Battery ${state}${time_est}\nCapacity: ${capacity_percent}%",
   alert_threshold = 5,
   alert_timeout = 0,
   alert_title = "Low battery !",
   alert_text = "${AC_BAT}${time_est}"
}

-- ALSA volume
local volicon = wibox.widget.imagebox(theme.widget_vol)
theme.volume = lain.widget.alsa({
      settings = function()
         if volume_now.status == "off" then
            volicon:set_image(theme.widget_vol_mute)
         elseif tonumber(volume_now.level) == 0 then
            volicon:set_image(theme.widget_vol_no)
         elseif tonumber(volume_now.level) <= 50 then
            volicon:set_image(theme.widget_vol_low)
         else
            volicon:set_image(theme.widget_vol)
         end

         widget:set_markup(markup.font(theme.font, " " .. volume_now.level .. "% "))
      end
})

-- Net
local neticon = wibox.widget.imagebox(theme.widget_net)
local net = lain.widget.net({
      settings = function()
         widget:set_markup(markup.font(theme.font,
                                       markup("#7AC82E", string.format("⇓%3.1f", net_now.received))
                                          .. " " ..
                                          markup("#46A8C3", string.format(" ⇑%3.1f", net_now.sent) .. " ")))
      end
})

-- -- Some tstatus information
-- local function check_caps()
--    local f = io.popen("xset -q | grep 'Caps Lock: *on'")
--    local capsstatus = f:read("*all")
--    f:close()
--    return capsstatus:len() > 0
-- end

-- local capslock_indicator = leds_widget({ led_name = "Capslock",
--                                          is_enabled = check_caps,
--                                          text_on = "Capslock",
--                                          text_off = "Capslock",
-- })

-- Separators
local spr     = wibox.widget.textbox(' ')
local arrl_dl = separators.arrow_left(theme.bg_focus, "alpha")
local arrl_ld = separators.arrow_left("alpha", theme.bg_focus)

function theme.at_screen_connect(s)
   -- Quake application
   s.quake = lain.util.quake({ app = awful.util.terminal })

   -- If wallpaper is a function, call it with the screen
   local wallpaper = theme.wallpaper
   if type(wallpaper) == "function" then
      wallpaper = wallpaper(s)
   end
   gears.wallpaper.maximized(wallpaper, s, true)

   -- Tags
   awful.tag(awful.util.tagnames, s, awful.layout.layouts)

   -- Create a promptbox for each screen
   s.mypromptbox = awful.widget.prompt()
   -- Create an imagebox widget which will contains an icon indicating which layout we're using.
   -- We need one layoutbox per screen.
   s.mylayoutbox = awful.widget.layoutbox(s)
   s.mylayoutbox:buttons(awful.util.table.join(
                            awful.button({ }, 1, function () awful.layout.inc( 1) end),
                            awful.button({ }, 3, function () awful.layout.inc(-1) end),
                            awful.button({ }, 4, function () awful.layout.inc( 1) end),
                            awful.button({ }, 5, function () awful.layout.inc(-1) end)))

   -- Create a taglist widget
   s.mytaglist = awful.widget.taglist(s, awful.widget.taglist.filter.all, awful.util.taglist_buttons)

   -- Create a tasklist widget
   s.mytasklist = awful.widget.tasklist(s, awful.widget.tasklist.filter.currenttags, awful.util.tasklist_buttons)

   -- Create the wibox
   s.mywibox = awful.wibar({ position = "top", screen = s, height = 20, bg = theme.bg_normal, fg = theme.fg_normal })

   -- Add widgets to the wibox
   s.mywibox:setup {
      layout = wibox.layout.align.horizontal,

      -- Left widgets
      {
         layout = wibox.layout.fixed.horizontal,
         --spr,
         s.mytaglist,
         s.mypromptbox,
         spr,
      },

      -- Middle widget
      s.mytasklist,

      -- Right widgets
      {
         layout = wibox.layout.fixed.horizontal,
         wibox.widget.systray(),
         spr,

         arrl_ld,
         wibox.container.background(volicon, theme.bg_focus),
         wibox.container.background(theme.volume.widget, theme.bg_focus),
         -- volicon,
         -- theme.volume.widget,

         arrl_dl,
         memicon,
         mem.widget,

         arrl_ld,
         wibox.container.background(cpuicon, theme.bg_focus),
         wibox.container.background(cpu.widget, theme.bg_focus),
         wibox.container.background(cpu_info.widget, theme.bg_focus),

         arrl_dl,
         tempicon,
         temp.widget,

         arrl_ld,
         wibox.container.background(fsicon, theme.bg_focus),
         -- wibox.container.background(theme.fs.widget, theme.bg_focus),

         arrl_dl,
         -- baticon,
         bat.widget,

         arrl_ld,
         wibox.container.background(neticon, theme.bg_focus),
         wibox.container.background(net.widget, theme.bg_focus),

         arrl_dl,
         clock,
         spr,

         arrl_ld,
         wibox.container.background(s.mylayoutbox, theme.bg_focus),
         -- wibox.container.background(capslock_indicator.widget, theme.bg_focus)
      },
   }
end

return theme
