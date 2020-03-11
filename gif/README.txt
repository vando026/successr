ffmpeg -ss 2 -t 10 -i input.mp4 -vf "fps=10,scale=1100:-1:flags=lanczos,split[s0][s1];[s0]palettegen[p];[s1][p]paletteuse" -loop 0 demo.gif
