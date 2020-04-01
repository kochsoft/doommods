# What is this?

An almost trivial Doom mod I have written for two reasons:

* I always wanted a ready-to-use light amp available. Guess I am pampered by Doom 3's duct-tape mode. The final kick to do this now was John Romero's e1m8b map with that very dark final battle. I'd like to see what is going on there!
* I also always wanted to once-in-my-life write a Doom Mod. Since I have little spare time and less artistic skill the K.I.S.S. principle guided the project.

Just for the heck of it I also added hotkeys for activating Berserk and Antirad modes. Actually ZDoom offers a lot more. However, for now I am contented. This Toolbox-Mod needs not to be that much of a cheat-fest.

# How to use this?

## Installation

It is a simple wad to be added to your (g)zdoom call. As a Linuxer say something like
```
./gzdoom lightamp.zip
```

On windows I hear that it suffices to drop the file on the doom executable. However, never witnessed that drag-n-drop magic myself.

The mod should play nice with other wads. The source is simple console scripting and I used my initials as prefix where I could to avoid collisions.

## Usage in Game

Under Customize Controls you will find the menu "MHK Helper Bag". Open it and assign keys to the offered functions.

Later, while playing, you can use these keys to trigger berserk, antirad suit and/or night vision mode. At this time only the night vision offers actual toggling. The other powerups are merely activated.

# Known Bugs

The binding state of the key for the night vision amp is saved when the program is closed. If this happens while night vision is activated then after the next start the toggle state of the console command and the powerup will no longer be in sync. The workaround is to press the toggle key twice.

# Sounds

I wanted _some_ sound. So I played a high D and a lower E powerchord on my old acoustic guitar and used these for on and off.
Lame as hell, I know. But free of charge! Feel free to replace by something of your own if you see any potential in further developing this most glorious mod!

# Special Thanks

Special thanks go to "Brohnesorge". He wrote the quite fulminant "Hearts of Demons - Baron" mod, which not only is enjoyable to play (and
recommended for anyone who loves an artistically well-done cross-over between "Russian Overkill" and "Diablo 2"-Sorceress-Magic, but which
also gave a template for this here mod. Actually the light amp is little more than a rip-off of Bohnesorge's "Baron Vision" code. Liking the
grayscales I did not even replace the color function.

Also many thanks to the authors of ZDoom.org's excellent Wiki pages!

======

Markus-Hermann Koch, April's fool day 2020.
