import matplotlib.pyplot as plt
from matplotlib.patches import Rectangle

chip8  = ['123C','456D','789E','A0BF']
qwerty = ['1234','QWER','ASDF','ZXCV']
qwertz = ['1234','QWER','ASDF','YXCV']
azerty = ['1234','AZER','QSDF','WXCV']

fig, axes = plt.subplots(nrows=1,ncols=3,figsize=(8,4),dpi=200)

for ax,keypad,title in zip(axes,[chip8,qwerty,azerty],["CHIP-8", "QWERTY", "AZERTY"]):

	ax.set_aspect('equal')

	w = 0.9
	h = 0.9

	for i, row in enumerate(keypad):
		for j, button in enumerate(row):

			x = j + w
			y = (2-i) + h

			cx = x + w/2.
			cy = y + h/2.

			b = Rectangle((x,y),w,h,facecolor='silver',edgecolor='black',fill=True,alpha=0.5)
			
			ax.add_patch(b)
			ax.annotate(button,(cx,cy),ha='center',va='center')


	ax.set_title(title)
	ax.axis('off')
	
	ax.relim()
	ax.autoscale_view()

fig.savefig("keypads.png",bbox_inches='tight')

plt.show()
