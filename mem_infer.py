import numpy as np
mat_t = np.array([
  [1, 0, 0],
  [0, 1, 0],
  [1, 1, 1]
])
mat_t = np.matrix(mat_t)
mat_x = np.array([
  [1, 0, 0],
  [0, 0, 1]
])
for i in range(5):

  st = np.array([1, 1, i])
  st=np.matrix(st)
  print(mat_x * mat_t.I * st.T)
