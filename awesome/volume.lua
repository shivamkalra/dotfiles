function volume_info(cmd)
   local fd = io.popen(cmd)
   local status = fd:read("*all")
   fd:close()

   -- local volume = tonumber(string.match(status, "(%d?%d?%d)%%")) / 100
   local volume = string.match(status, "(%d?%d?%d)%%")
   volume = string.format("%3d", volume)

   status = string.match(status, "%[(o[^%]]*)%]")

   if string.find(status, "on", 1, true) then
       -- For the volume numbers
       volume = volume .. "%"
   else
       -- For the mute button
       volume = volume .. "M"
   end

   volume = volume:gsub("^%s*(.-)%s*$", "%1")
   return '<span font="monospace 9" color=\"#6A8587\">VOL:</span><span font="monospace 9">'..volume..'</span> '
end
