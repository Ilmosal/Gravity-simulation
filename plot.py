from matplotlib import pyplot
import pylab
import numpy
from mpl_toolkits.mplot3d import Axes3D
import sys

try:
        filename = sys.argv[1]
except:
        sys.exit('No argument')

try:
        f = open(filename, 'r')
except:
        sys.exit('No such file')

objects = int(f.readline())
steps = int(f.readline())

x = numpy.zeros(objects)
y = numpy.zeros(objects)
z = numpy.zeros(objects)

fig = pylab.figure(figsize=(10,10))

for i in xrange(1, steps):
        for j in xrange(0, objects):
                x[j] = float(f.readline())
                y[j] = float(f.readline())
                z[j] = float(f.readline())

        ax=Axes3D(fig)
        ax.set_xlim3d(-7,7)
        ax.set_ylim3d(-7,7)
        ax.set_zlim3d(-1,1)
        ax.scatter(x,y,z,s=10)

        if (i < 10):
                pyplot.savefig('pictures/fig000%d.png' %i)
        elif (i < 100):
                pyplot.savefig('pictures/fig00%d.png' %i)
        elif (i < 1000):
                pyplot.savefig('pictures/fig0%d.png' %i)
        else:
                pyplot.savefig('pictures/fig%d.png' %i)

        fig.clf()

pyplot.close(fig)
