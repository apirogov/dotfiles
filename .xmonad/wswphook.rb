#!/usr/bin/env ruby
#Workspace Wallpaper Hook
#LogHook script to set the correct wallpapers for each workspace
#If the visible workspaces change in some way, updates wallpaper
#Automagically rotates 90 degrees for vertical screens, renders scaled
#Works with feh and imagemagick, workspace names should start with number
#Copyright (C) 2014 Anton Pirogov

#Path of the pictures to be used, must contain N.{jpg,png,..} files of folders named N containing
#pics which are chosen randomly, where N=number of workspace
PIC_PATH="~/myfiles/pictures/Wallpapers/workspaces/"

exit 1 if !File.exists?(File.expand_path(PIC_PATH))

#get infos about connected monitors (w,h,x,y) and virtual w,h size
$xrandr = `xrandr | grep ".* connected .*"`.split("\n").map{|l| l.split(' ')[2].split(/\D+/).map(&:to_i)}
$vx,$vy = `xrandr | grep current`.split(' ')[7..9].join.split("x").map(&:to_i)

def esc(str); str.gsub(' ',"\\ "); end #Escape whitespace

#get picture resolution
def get_res(pic)
  `identify #{esc pic}`.split(/\s+/)[2].split("x").map(&:to_i)
end

#get pic for workspace.
def get_pic_path(workspace)
  path = Dir.entries(File.expand_path PIC_PATH).select{|l| l.match /#{workspace[0]}.*/}.first
  return nil if !path #no match

  path = File.expand_path(PIC_PATH)+'/'+path
  if Dir.exists? path  # is directory? choose random picture
    pics = Dir.entries(path)-['.','..']
    pic = pics.shuffle.first
    return path+"/"+pic
  end
  return path # Return picture
end

old=nil
line="x"
while line!=nil && line.chomp.length>0
  #workspaces = STDIN.gets.scan(/\[\S*\]/).to_a.map{|x| x[1..-2]} #with mostly default hook
  workspaces = STDIN.gets #with stripped dynamicLogPP

  if workspaces != old
    # `echo '#{workspaces}' >> /home/admin/debug`
    old = workspaces

    layers = ''
    workspaces = workspaces.split(',').each_with_index do |ws,i|
      pic = get_pic_path ws
      next if pic.nil? #no picture for workspace -> no layer for it

      px,py = get_res pic
      pratio = px.to_f/py
      w,h,x,y = $xrandr[i]
      wratio = w.to_f/h
      rotate = wratio > 1 && pratio < 1 || wratio < 1 && pratio > 1

      layers += "\\( '#{esc pic}' #{rotate ? "-rotate 90" : ''} -scale #{w}x#{h}! \\) -geometry +#{x}+#{y} -composite "
    end
    cmd = "convert -size #{$vx}x#{$vy} xc:black #{layers} jpg:- | feh --no-xinerama --bg-tile --no-fehbg -"
    # `echo '#{cmd}' >> /home/admin/debug`
    `#{cmd}`
  end
end
