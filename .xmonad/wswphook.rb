#!/usr/bin/env ruby
#Workspace Wallpaper Hook
#LogHook script to set the correct wallpapers for each workspace
#If the visible workspaces change in some way, updates wallpaper
#Works with wallpaperd or feh
#Copyright (C) 2013 Anton Pirogov

useFeh = true
#Path of the pictures to be used by feh, must contain N.jpg files
#where n=number of workspace
PIC_PATH="~/myfiles/pictures/funktional/Wallpapers/workspaces/"

#if feh ist not used, configure wallpaperd accordingly (do it! it's faster!)
#and this script will just forward the data to wallpaperd

exit 1 if useFeh && !File.exists?(File.expand_path(PIC_PATH))

old=nil
line="x"
while line!=nil && line.chomp.length>0
  #workspaces = STDIN.gets.scan(/\[\S*\]/).to_a.map{|x| x[1..-2]} #with mostly default hook
  workspaces = STDIN.gets #with stripped dynamicLogPP

  if workspaces != old
    if useFeh
      cmd = "feh --no-fehbg --bg-scale #{workspaces.split(',').map{|x| PIC_PATH+x[0]+'.jpg'}.join ' '}"
    else
      cmd = "wallpaperd --visible-workspaces #{workspaces}"
    end
    `#{cmd}`

    old = workspaces
  end
end
