#!/usr/bin/env ruby
#Workspace Wallpaper Hook
#LogHook script to set the correct wallpapers for each workspace
#If the visible workspaces change in some way, updates wallpaper
#Works with wallpaperd or feh
#Copyright (C) 2013 Anton Pirogov

setter = :feh

#Path of the pictures to be used, must contain N.jpg files of folders named N containing
#pics which are chosen randomly, where N=number of workspace
PIC_PATH="~/myfiles/pictures/Wallpapers/workspaces/"

exit 1 if !File.exists?(File.expand_path(PIC_PATH))

def get_pic_path(workspace)
  path = File.expand_path (PIC_PATH+workspace[0])

  if Dir.exists? path  # has directory? choose random one
    pics = Dir.entries path
    pic = pics.shuffle.first
    return path+"/"+pic
  end

  # Return <ws number>.jpg path
  return path+".jpg"
end

old=nil
line="x"
while line!=nil && line.chomp.length>0
  #workspaces = STDIN.gets.scan(/\[\S*\]/).to_a.map{|x| x[1..-2]} #with mostly default hook
  workspaces = STDIN.gets #with stripped dynamicLogPP

  if workspaces != old
    cmd = case setter
    when :feh then "feh --no-fehbg --bg-scale #{workspaces.split(',').map{|x| get_pic_path x}.join ' '}"
    else "wallpaperd --visible-workspaces #{workspaces}"
    end

    `#{cmd}`

    old = workspaces
  end
end
