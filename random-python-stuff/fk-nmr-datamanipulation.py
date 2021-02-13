# coding: utf8
import numpy as np

data = np.genfromtxt('nmr.dat', dtype=float, delimiter=None)

#umwandeln der ersten Spalte, umwandeln in Spaltenvektor für concat
a_row = data[:,0] / 79.5284
a_col = a_row.reshape(-1,1)


rest = data[:,1:]


#zusammenfügen, Spalten durch axis=1
newData = np.concatenate((a_col,rest), axis=1)

#speichern als nmr_neu.dat
np.savetxt('nmr_neu.dat', newData, delimiter='\t')
