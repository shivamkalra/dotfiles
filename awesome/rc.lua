-- Standard awesome library
local gears = require("gears")
local awful = require("awful")
awful.rules = require("awful.rules")
require("awful.autofocus")
vicious = require("vicious")
-- Widget and layout library
local wibox = require("wibox")
-- Theme handling library
local beautiful = require("beautiful")
-- Notification library
local naughty = require("naughty")
local menubar = require("menubar")
local battery = require("battery")
local volume = require("volume")
local sexec = awful.util.spawn_with_shell

-- {{{ Globals
widget_pid_t = {}
-- }}}

-- {{{ Utility functions
function run_if_not_exist_in_tag(prg, tagidx)
  local handle = io.popen(string.format("ps aux | grep [%s]%s",
                                        string.sub(prg, 1, 1),
                                        string.sub(prg, 2)))
  local result = handle:read("*a")
  handle:close()
  if result == nil or result == '' then
    awful.util.spawn(prg)
  end
  -- assumption: user trying open in focused screen
  awful.tag.viewonly(tags[mouse.screen][tagidx])
end

-- {{{ Error handling
-- Check if awesome encountered an error during startup and fell back to
-- another config (This code will only ever execute for the fallback config)
if awesome.startup_errors then
  naughty.notify({ preset = naughty.config.presets.critical,
                   title = "Oops, there were errors during startup!",
                   text = awesome.startup_errors })
end

-- Handle runtime errors after startup
do
  local in_error = false
  awesome.connect_signal(
    "debug::error",
    function (err)
      -- Make sure we don't go into an endless error loop
      if in_error then return end
      in_error = true

      naughty.notify({ preset = naughty.config.presets.
                         critical,
                       title = "Oops, an error"..
                         "happened!",
                       text = err })
      in_error = false
  end)
end
-- }}}

-- {{{ Variable definitions
-- Themes define colours, icons, and wallpapers
beautiful.init( awful.util.getdir("config") ..
                  "/themes/awesome-solarized/dark/theme.lua" )
theme.wallpaper = awful.util.getdir("config") .. "/themes/wallpaper.jpg"
awesome.font = "monospace 9"
theme.font = "monospace 9"

local email = require("email")
-- This is used later as the default terminal and editor to run.
terminal = "termite"
editor = os.getenv("EDITOR") or "emacs"
editor_cmd = terminal .. " -e " .. editor

-- Default modkey.
-- Usually, Mod4 is the key with a logo between Control and Alt.
-- If you do not like this or do not have such a key,
-- I suggest you to remap Mod4 to another key using xmodmap or other tools.
-- However, you can use another modifier like Mod1, but it may interact
-- with others.
modkey = "Mod4"


-- Table of layouts to cover with awful.layout.inc, order matters.
-- Types of laouts:
-- colums rows magnified maximized spiral zig-zag floating

local layouts =
  {
    awful.layout.suit.floating,
    awful.layout.suit.tile,
    awful.layout.suit.max
  }
-- }}}

-- {{{ Wallpaper
if beautiful.wallpaper then
  for s = 1, screen.count() do
    gears.wallpaper.maximized(beautiful.wallpaper, s, true)
  end
end
-- }}}

-- {{{ Tags
-- Define a tag table which hold all screen tags.
tags = {
  names = { "web", "emacs", "term", "work", "im", "read", 7, 8, 9 },
  layout = {
    layouts[2], layouts[2], layouts[3], layouts[2], layouts[2],
    layouts[2], layouts[2], layouts[1], layouts[1]
  }
}

for s = 1, screen.count() do
  -- Each screen has its own tag table.
  tags[s] = awful.tag(tags.names, s, tags.layout)
end

-- }}}

-- {{{ Menu
-- Create a laucher widget and a main menu
myawesomemenu = {
  { "manual", terminal .. " -e man awesome" },
  { "edit config", editor_cmd .. " " .. awesome.conffile },
  { "restart", awesome.restart },
  { "quit", awesome.quit }
}

mymainmenu = awful.menu(
  { items = {
      { "awesome", myawesomemenu, beautiful.awesome_icon },
      { "open terminal", terminal } } }
)

mylauncher = awful.widget.launcher({ image = beautiful.awesome_icon,
                                     menu = mymainmenu })

-- Menubar configuration
-- Set the terminal for applications that require it
menubar.utils.terminal = terminal
-- }}}

-- {{{ Wibox
-- Create a textclock widget
textclock_widget = awful.widget.textclock("<span font=\"monospace 9\"> %a %b %d, %I:%M %p</span> ", 60)
weather_t = awful.tooltip({ objects = { weatherwidget },})
-- create a battery widget
battery_widget = wibox.widget.textbox()
email_widget = wibox.widget.textbox()

-- create a volume widget
volume_widget = wibox.widget.textbox()
-- Register buttons
volume_widget:buttons( awful.button({ }, 1, function () awful.util.spawn("pavucontrol") end) )
volume_widget:set_markup(volume_info("amixer sget Master"))

local wsep = "<span font=\"monospace 9\" color=\"#6A8587\">|</span>"
-- Network Widget
local netwidget = wibox.widget.textbox()
vicious.register(netwidget, vicious.widgets.net, ' <span font="monospace 9" color="#CC9393">D:${wlp3s0 down_kb}</span> <span font="monospace 9" color="#7F9F7F">U:${wlp3s0 up_kb}</span> ' .. wsep .. ' ', 1)
netwidget:buttons(
  awful.button({},
    1,
    function ()
      local pid = awful.util.spawn(terminal .. " -e netmonitor")
      widget_pid_t[pid] = function (c)
        awful.client.moveresize(0, 0, 200, 200, c)
      end
    end
))


-- Memory Widget
local memwidget = wibox.widget.textbox()
-- vicious.cache(vicious.widgets.mem)
vicious.register(memwidget, vicious.widgets.mem, "<span font=\"monospace 9\" color=\"#6A8587\">MEM:</span><span font=\"monospace 9\">$1% ($2 MB)</span> ", 9)
memwidget:buttons(
  awful.button({},
    1,
    function ()
      local pid = awful.util.spawn(terminal .. " -e vtop -t becca")
      widget_pid_t[pid] = function (c)
        awful.client.moveresize(0, 0, 200, 200, c)
      end
    end
))

-- Weather widget
local weatherwidget = wibox.widget.textbox()
weather_t = awful.tooltip({ objects = { weatherwidget },})

vicious.register(
  weatherwidget,
  vicious.widgets.weather,
  function (widget, args)
    weather_t:set_text("City: " .. args["{city}"]
                         .."\nWind: " .. args["{windkmh}"]
                         .. "km/h " .. args["{wind}"] .. "\nSky: "
                         .. args["{sky}"] .. "\nHumidity: "
                         .. args["{humid}"] .. "%")
    return "  <span font=\"monospace 9\" color=\"#6A8587\">TEMP:</span><span font=\"monospace 9\"> " .. args["{tempc}"] .. "°C</span> " .. wsep .. " "
  end, 300, "CYYZ")
weatherwidget:buttons(
  awful.button({ }, 1,
    function ()
      local pid = awful.util.spawn(terminal .. ' -e sh -c "wego 5 | less"')
      widget_pid_t[pid] = function (c)
        awful.client.moveresize(400, 0, 400, 550, c)
      end
    end
))

-- CPU Widget
-- Initialize widget
-- Initialize widget
local cpuwidget = wibox.widget.textbox()
cpuwidget_t = awful.tooltip({ objects = { cpuwidget },})
-- Initialize widgets
vicious.register(
  cpuwidget,
  vicious.widgets.cpu,
  function (widget, args)
    local text
    -- list all cpu cores
    for i=1,#args do
      -- append to list
      if i >= 2 then text = text .. 'Cpu ' .. i-1 .. ': ' .. args[i] .. '%\n'
      else text = 'Overall: ' .. args[i] .. '%\n' end
    end

    cpuwidget_t:set_text(text)
    return '<span font=\"monospace 9\" color=\"#6A8587\">CPU:</span><span font=\"monospace 9\">' .. args[1] .. '%</span> ' .. wsep
  end, 7)

-- Register buttons
cpuwidget:buttons(
  awful.button({},
    1,
    function ()
      local pid = awful.util.spawn(terminal .. " -e htop")
      widget_pid_t[pid] = function (c)
        awful.client.moveresize(0, 0, 100, 100, c)
      end
    end
))

-- Create a wibox for each screen and add it
mywibox = {}
mypromptbox = {}
mylayoutbox = {}
mytaglist = {}
mytaglist.buttons = awful.util.table.join(
  awful.button({ }, 1, awful.tag.viewonly),
  awful.button({ modkey }, 1, awful.client.movetotag),
  awful.button({ }, 3, awful.tag.viewtoggle),
  awful.button({ modkey }, 3, awful.client.toggletag),
  awful.button({ }, 4, function(t)
      awful.tag.viewnext(awful.tag.getscreen(t))
  end),
  awful.button({ }, 5, function(t)
      awful.tag.viewprev(awful.tag.getscreen(t))
  end)
)
mytasklist = {}
mytasklist.buttons = awful.util.table.join(
  awful.button({ }, 1, function (c)
      if c == client.focus then
        c.minimized = true
      else
        -- Without this, the following
        -- :isvisible() makes no sense
        c.minimized = false
        if not c:isvisible() then
          awful.tag.viewonly(c:tags()[1])
        end
        -- This will also un-minimize
        -- the client, if needed
        client.focus = c
        c:raise()
      end
  end),
  awful.button({ }, 3, function ()
      if instance then
        instance:hide()
        instance = nil
      else
        instance = awful.menu.clients({ width=250 })
      end
  end),
  awful.button({ }, 4, function ()
      awful.client.focus.byidx(1)
      if client.focus then client.focus:raise() end
  end),
  awful.button({ }, 5, function ()
      awful.client.focus.byidx(-1)
      if client.focus then client.focus:raise() end
end))

for s = 1, screen.count() do
  -- Create a promptbox for each screen
  mypromptbox[s] = awful.widget.prompt()
  -- Create an imagebox widget which will contains an icon indicating which
  -- layout we're using.
  -- We need one layoutbox per screen.
  mylayoutbox[s] = awful.widget.layoutbox(s)
  mylayoutbox[s]:buttons(awful.util.table.join(
                           awful.button({ }, 1, function ()
                               awful.layout.inc(layouts, 1) end),
                           awful.button({ }, 3, function ()
                               awful.layout.inc(layouts, -1) end),
                           awful.button({ }, 4, function ()
                               awful.layout.inc(layouts, 1) end),
                           awful.button({ }, 5, function ()
                               awful.layout.inc(layouts, -1) end)
                        ))
  -- Create a taglist widget
  mytaglist[s] = awful.widget.taglist(s, awful.widget.taglist.filter.all,
                                      mytaglist.buttons)

  -- Create a tasklist widget
  mytasklist[s] = awful.widget.tasklist(s, awful.widget.tasklist.filter.
                                          currenttags, mytasklist.buttons)

  -- Create the wibox
  mywibox[s] = awful.wibox({ position = "top", screen = s, height = 16 })

  -- Widgets that are aligned to the left
  local left_layout = wibox.layout.fixed.horizontal()
  left_layout:add(mylauncher)
  left_layout:add(mytaglist[s])
  left_layout:add(mypromptbox[s])

  -- Widgets that are aligned to the right
  local right_layout = wibox.layout.fixed.horizontal()
  right_layout:add(email_widget)
  right_layout:add(weatherwidget)
  right_layout:add(memwidget)
  right_layout:add(cpuwidget)
    right_layout:add(netwidget)
  right_layout:add(volume_widget)
  right_layout:add(battery_widget)
  if s == 1 then right_layout:add(wibox.widget.systray()) end
  right_layout:add(textclock_widget)
  right_layout:add(mylayoutbox[s])


  -- Now bring it all together (with the tasklist in the middle)
  local layout = wibox.layout.align.horizontal()
  layout:set_left(left_layout)
  layout:set_middle(mytasklist[s])
  layout:set_right(right_layout)


  mywibox[s]:set_widget(layout)
end

-- }}}

-- {{{ Mouse bindings
root.buttons(
  awful.util.table.join(
    awful.button({ }, 3, function () mymainmenu:toggle() end),
    awful.button({ }, 4, awful.tag.viewnext),
    awful.button({ }, 5, awful.tag.viewprev)
))
-- }}}

-- {{{ Key bindings
globalkeys = awful.util.table.join(
  awful.key({ }, "XF86AudioRaiseVolume",
    function ()
      volume_widget:set_markup(volume_info("amixer set Master 1%+"))
  end),
  awful.key({modkey,            }, "F1",     function () awful.screen.focus(2) end),
  awful.key({modkey,            }, "F2",     function () awful.screen.focus(1) end),
  awful.key({ }, "XF86AudioLowerVolume", function ()
      volume_widget:set_markup(volume_info("amixer set Master 1%-"))
  end),
  awful.key({ }, "XF86AudioMute", function ()
      volume_widget:set_markup(volume_info("pactl set-sink-mute 0 toggle; amixer"))
  end),
  -- awful.key({ modkey }, "Home", function()
  --    awful.util.spawn("xbacklight -inc 5", false)
  -- end),
  -- awful.key({ modkey }, "End", function()
  --    awful.util.spawn("xbacklight -dec 5", false)
  -- end),
  awful.key({ modkey, }, "Left", awful.tag.viewprev ),
  awful.key({ modkey, }, "Right", awful.tag.viewnext ),
  awful.key({ modkey, "Mod1"  }, "Right", function () awful.tag.incmwfact( 0.01)   end),
  awful.key({ modkey, "Mod1"  }, "Left",  function () awful.tag.incmwfact(-0.01)   end),
  awful.key({ modkey, "Shift" }, "Down",  function () awful.client.incwfact( 0.01) end),
  awful.key({ modkey, "Shift" }, "Up",    function () awful.client.incwfact(-0.01) end),
  awful.key({ modkey, "Shift"   }, "Left",
    function (c)
      local curidx = awful.tag.getidx()
      if curidx == 1 then
        awful.client.movetotag(tags[math.floor(mouse.screen)][9])
      else
         awful.client.movetotag(tags[mouse.screen][curidx - 1])
      end
  end),
  awful.key({ modkey, "Shift"   }, "Right",
    function (c)
      local curidx = awful.tag.getidx()
      if curidx == 9 then
        awful.client.movetotag(tags[client.focus.screen][1])
      else
        awful.client.movetotag(tags[client.focus.screen][curidx + 1])
      end
  end),
  awful.key({ modkey, }, "Escape", awful.tag.history.restore),
  -- Tag movements
  -- open emacs Mod + e
  awful.key({ modkey, }, "e",
    function (c)
      run_if_not_exist_in_tag("emacs", 2)
      -- well this is debatable which screen emacs exists?
  end),
  -- youtube search
  awful.key({ modkey, }, "y",
    function (c) sexec('yts') end),
  awful.key({ modkey, "Shift" }, "y",
    function (c)
      naughty.notify({ text = 'Sending link to mpv...',
                       timeout=2,
                       screen=mouse.screen})
      sexec('mpv $(xclip -o)')
  end),
  awful.key({ modkey, "Shift"}, "z",
    function (c)
      awful.util.spawn("clipmenu")
    end
  ),
  -- open chrome with Mod + b
  awful.key({ modkey, }, "b",
    function (c)
      run_if_not_exist_in_tag("chromium", 1)
  end),
  awful.key({ modkey, "Shift"   }, ",",
    function (c)
      local curidx = awful.tag.getidx()
      if curidx == 1 then
        awful.client.movetotag(tags[client.focus.screen][9])
      else
        awful.client.movetotag(tags[client.focus.screen][curidx - 1])
      end
  end),
  awful.key({ modkey, "Shift"   }, ".",
    function (c)
      local curidx = awful.tag.getidx()
      if curidx == 9 then
        awful.client.movetotag(tags[client.focus.screen][1])
      else
        awful.client.movetotag(tags[client.focus.screen][curidx + 1])
      end
  end),
  awful.key({ modkey, }, "j",
    function ()
      awful.client.focus.byidx( 1)
      if client.focus then client.focus:raise() end
  end),
  awful.key({ modkey, }, "k",
    function ()
      awful.client.focus.byidx(-1)
      if client.focus then client.focus:raise() end
  end),
  awful.key({ modkey, }, "w", function () mymainmenu:show() end),

  -- Layout manipulation
  awful.key({ modkey, "Shift" }, "j",
    function ()
      awful.client.swap.byidx( 1)
  end),
  awful.key({ modkey, "Shift" }, "k",
    function ()
      awful.client.swap.byidx( -1)
  end),
  awful.key({ modkey, "Control" }, "j",
    function ()
      awful.screen.focus_relative( 1)
  end),
  awful.key({ modkey, "Control" }, "k",
    function ()
      awful.screen.focus_relative(-1)
  end),
  awful.key({ modkey, }, "u", awful.client.urgent.jumpto),
  awful.key({ modkey, }, "Tab",
    function ()
      awful.client.focus.history.previous()
      if client.focus then
        client.focus:raise()
      end
  end),

  -- Standard program
  awful.key({ modkey, }, "Return", function ()
      awful.util.spawn(terminal) end),
  awful.key({ modkey, "Control" }, "r", awesome.restart),
  awful.key({ modkey, "Shift" }, "q", awesome.quit),

  awful.key({ modkey, }, "l", function ()
      awful.tag.incmwfact( 0.05) end),
  awful.key({ modkey, }, "h", function ()
      awful.tag.incmwfact(-0.05) end),
  awful.key({ modkey, "Shift" }, "h", function ()
      awful.tag.incnmaster( 1) end),
  awful.key({ modkey, "Shift" }, "l", function ()
      awful.tag.incnmaster(-1) end),
  awful.key({ modkey, "Control" }, "h", function ()
      awful.tag.incncol( 1) end),
  awful.key({ modkey, "Control" }, "l", function ()
      awful.tag.incncol(-1) end),
  awful.key({ modkey, }, "space", function ()
      awful.layout.inc(layouts, 1) end),
  awful.key({ modkey, "Shift" }, "space", function ()
      awful.layout.inc(layouts, -1) end),

  awful.key({ modkey, "Control" }, "n", awful.client.restore),

  -- Prompt
  awful.key({ modkey }, "r", function () mypromptbox[mouse.screen]:run() end),

  awful.key({ modkey }, "x",
    function ()
      awful.prompt.run({ prompt = "Run Lua code: " },
        mypromptbox[mouse.screen].widget,
        awful.util.eval, nil,
        awful.util.getdir("cache") .. "/history_eval")
  end),
  -- Menubar
  awful.key({ modkey }, "p", function() sexec('dmenu-frecency') end)
)

clientkeys = awful.util.table.join(
  awful.key({ modkey, }, "f", function (c)
      c.fullscreen = not c.fullscreen end),
  awful.key({ modkey, "Shift" }, "c", function (c)
      c:kill() end),
  awful.key({ modkey, "Control" }, "space", awful.client.floating.toggle),
  awful.key({ modkey, "Control" }, "Return", function (c)
      c:swap(awful.client.getmaster()) end),
  awful.key({ modkey, }, "o", awful.client.movetoscreen),
  awful.key({ modkey, }, "t", function (c)
      c.ontop = not c.ontop end),
  awful.key({ modkey, }, "n",
    function (c)
      -- The client currently has the input focus, so it cannot be
      -- minimized, since minimized clients can't have the focus.
      c.minimized = true
  end),
  awful.key({ modkey, }, "m",
    function (c)
      c.maximized_horizontal = not c.maximized_horizontal
      c.maximized_vertical = not c.maximized_vertical
  end)
)

-- Bind all key numbers to tags.
-- Be careful: we use keycodes to make it works on any keyboard layout.
-- This should map on the top row of your keyboard, usually 1 to 9.
for i = 1, 9 do
  globalkeys = awful.util.
    table.join(
      globalkeys,
      awful.key({ modkey }, "#" .. i + 9,
        function ()
          local screen = mouse.screen
          local tag = awful.tag.gettags(screen)[i]
          if tag then
            awful.tag.viewonly(tag)
          end
      end),
      awful.key({ modkey, "Control" }, "#" .. i + 9,
        function ()
          local screen = mouse.screen
          local tag = awful.tag.gettags(screen)[i]
          if tag then
            awful.tag.viewtoggle(tag)
          end
      end),
      awful.key({ modkey, "Shift" }, "#" .. i + 9,
        function ()
          if client.focus then
            local tag = awful.tag.gettags(client.focus.screen)[i]
            if tag then
              awful.client.movetotag(tag)
            end
          end
      end),
      awful.key({ modkey, "Control", "Shift" }, "#" .. i + 9,
        function ()
          if client.focus then
            local tag = awful.tag.gettags(client.focus.screen)[i]
            if tag then
              awful.client.toggletag(tag)
            end
          end
    end))
end

clientbuttons = awful.util.table.join(
  awful.button({ }, 1, function (c) client.focus = c; c:raise() end),
  awful.button({ modkey }, 1, awful.mouse.client.move),
  awful.button({ modkey }, 3, awful.mouse.client.resize))

-- Set keys
root.keys(globalkeys)
-- }}}

-- {{{ Rules
awful.rules.rules = {
    { rule = { },
    properties = { border_width = 1,
                   focus = awful.client.focus.filter,
                   keys = clientkeys,
                   buttons = clientbuttons },
    callback = function(c)
      if widget_pid_t[c.pid] ~= nil then
        c.ontop = true
        c.floating = true
        widget_pid_t[c.pid](c)
      end
    end
  },
  -- All clients will match this rule.
  { rule = { class = "MPlayer" },
    properties = { floating = true } },
  { rule = { class = "URxvt" },
    properties = { size_hints_honor = true },
    callback = function(c, t)
      local stagname = c:tags()[1].name
      if widget_pid_t[c.pid] == nil and stagname ~= "term" then
        c.ontop = true
        awful.client.floating.set(c, true)
        awful.client.moveresize( 20,  20, -40, -40)
        awful.placement.centered(c,nil)
      end
    end
  },
  { rule = { class = "Emacs", instance = "emacs" },
    -- to have small border between speedbar and emacs
    properties = { size_hints_honor = false}},
  { rule = { class = "pinentry" },
    properties = { floating = true } },
  { rule = { class = "gimp" },
    properties = { floating = true } },
  { rule = { class = "Pavucontrol" },
    properties = { floating = true } ,
    callback = function (c)
      awful.placement.centered(c,nil)
    end
  }
  -- Set Firefox to always map on tags number 2 of screen 1.
  -- { rule = { class = "Firefox" },
  -- properties = { tag = tags[1][2] } },
}
-- }}}

-- {{{ Signals
-- Signal function to execute when a new client appears.

battery_widget_timer = timer({timeout = 1})
battery_widget_timer:connect_signal(
  "timeout",
  function()
    battery_widget:set_markup(battery_info("BAT0"))
end)

battery_widget_timer:start()
battery_widget:set_markup(battery_info("BAT0"))

email_widget_timer = timer({timeout = 30})
email_widget_timer:connect_signal(
  "timeout",
  function()
    email_widget:set_markup(email_info())
end)

email_widget_timer:start()
email_widget:set_markup(email_info())
email_widget:buttons(
  awful.button({},
    1,
    function ()
      --awful.tag.viewonly(tags[1][2])
      awful.util.spawn('unreademails')
    end
))

client.connect_signal(
  "manage",
  function (c, startup)
    -- Enable sloppy focus
    c:connect_signal(
      "mouse::enter",
      function(c)
        if awful.layout.get(c.screen) ~=
          awful.layout.suit.magnifier
          and
        awful.client.focus.filter(c) then
          client.focus = c
        end
      end
    )

    if not startup then
      -- Set the windows at the slave,
      -- i.e. put it at the end of others instead of
      -- setting it master.
      -- awful.client.setslave(c)

      -- Put windows in a smart way, only if they does not
      -- set an initial position.
      if not c.size_hints.user_position and
        not c.size_hints.program_position
      then
        awful.placement.no_overlap(c)
        awful.placement.no_offscreen(c)
      end
    end

    local titlebars_enabled = false
    if titlebars_enabled and (c.type == "normal" or
                                c.type == "dialog")
    then
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

      -- Widgets that are aligned to the left
      local left_layout = wibox.layout.fixed.horizontal()
      left_layout:add(awful.titlebar.widget.iconwidget(c))
      left_layout:buttons(buttons)

      -- Widgets that are aligned to the right
      local right_layout = wibox.layout.fixed.horizontal()
      right_layout:add(awful.titlebar.widget.floatingbutton(c))
      right_layout:add(awful.titlebar.widget.maximizedbutton(c))
      right_layout:add(awful.titlebar.widget.stickybutton(c))
      right_layout:add(awful.titlebar.widget.ontopbutton(c))
      right_layout:add(awful.titlebar.widget.closebutton(c))

      -- The title goes in the middle
      local middle_layout = wibox.layout.flex.horizontal()
      local title = awful.titlebar.widget.titlewidget(c)
      title:set_align("center")
      middle_layout:add(title)
      middle_layout:buttons(buttons)

      -- Now bring it all together
      local layout = wibox.layout.align.horizontal()
      layout:set_left(left_layout)
      layout:set_right(right_layout)
      layout:set_middle(middle_layout)

      awful.titlebar(c):set_widget(layout)
    end
end)

client.connect_signal("focus", function(c) c.border_color = beautiful.
                        border_focus end)
client.connect_signal("unfocus", function(c) c.border_color = beautiful.
                        border_normal end)
-- }}}

-- {{{
-- Auto start
function run_once(prg)
  if not prg then
    do return nil end
  end
  sexec("pgrep -u $USER -x " .. prg .. " || (" .. prg .. ")")
end
