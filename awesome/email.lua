local naughty = require("naughty")
local json = require("cjson")
local beautiful = require("beautiful")
local os = require("os")
local last_notify_time = nil
local max_time_diff = 60*10

function email_info()
  -- local unreadmail = io.popen("notmuch count tag:unread")
  -- local ucount = unreadmail:read()
  -- unreadmail:close()

  -- if tonumber(ucount) > 0 then
  --   if last_notify_time == nil or os.difftime(os.time(), last_notify_time) > max_time_diff then
  --     last_notify_time = os.time()
  --     local info = ""
  --     local f = io.popen("notmuch search --sort=newest-first --format=json tag:unread")
  --     local out = f:read("*all")
  --     f:close()
  --     local threads = json.decode(out)

  --     local num, thread
  --     for num,thread in pairs(threads) do
  --       local date = thread["date_relative"]
  --       local subject = thread["subject"]
  --       subject = string.gsub(subject, "&","&")
  --       subject = string.gsub(subject, "<","<")
  --       subject = string.gsub(subject, ">",">")
  --       local authors = thread["authors"]
  --       authors = string.gsub(authors, "<(.*)>","")
  --       local tags = table.concat(thread["tags"],', ')

  --       if string.len(subject) > 120 then
  --         subject = string.sub(subject, 120) .. "... "
  --       end
  --       info =  info .. date .. ": " .. authors .. subject .. '\n'
  --     end

  --     naughty.notify({ title    = ucount .. " Unread Emails"
  --            , text   = info
  --            , timeout  = 100
  --            , position = "top_right"
  --       })
  --   end
  -- end
  return ' <span font="monospace 9" color=\"#6A8587\">MAIL:</span><span font=\"monospace 9\">2</span>'
end
