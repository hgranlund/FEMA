def idMatx(size):
  id = []
  for i in range(size):
    id.append([0]*size)
  for i in range(size):
    id[i][i] = 1
  return(id)
 
def tranMtx(inMtx):
  tMtx = []
  for row in range(0, len(inMtx[0])):
    tRow = []
    for col in range(0, len(inMtx)):
      ele = inMtx[col][row]
      tRow.append(ele)
    tMtx.append(tRow)
  return(tMtx)
 
def matxRound(matx, decPts=4):
  for col in range(len(matx)):
    for row in range(len(matx[0])):
      matx[col][row] = round(matx[col][row], decPts)
 
# the solver ...
 
def gj_Solve(A, b=False, decPts=4):
  """ A gauss-jordan method to solve an augmented matrix for
030
      the unknown variables, x, in Ax = b.
031
      The degree of rounding is 'tuned' by altering decPts = 4.
032
      In the case where b is not supplied, b = ID matrix, and therefore
033
      the output is the inverse of the A matrix.
034
  """
 
  if not b == False:
    # first, a test to make sure that A and b are conformable
    if (len(A) != len(b)):
      print 'A and b are not conformable'
      return
    Ab = A[:]
    Ab.append(b)
    m = tranMtx(Ab)
  else:
    ii = idMatx(len(A))
    Aa = A[:]
    for col in range(len(ii)):
      Aa.append(ii[col])
    tAa = tranMtx(Aa)
    m = tAa[:]
 
  (eqns, colrange, augCol) = (len(A), len(A), len(m[0]))
 
  # permute the matrix -- get the largest leaders onto the diagonals
  # take the first row, assume that x[1,1] is largest, and swap if that's not true
  for col in range(0, colrange):
    bigrow = col
    for row in range(col+1, colrange):
      if abs(m[row][col]) > abs(m[bigrow][col]):
        bigrow = row
        (m[col], m[bigrow]) = (m[bigrow], m[col])
  print 'm is ', m
 

  # reduce, such that the last row is has at most one unknown

  for rrcol in range(0, colrange):

    for rr in range(rrcol+1, eqns):
      cc = -(float(m[rr][rrcol])/float(m[rrcol][rrcol]))

      for j in range(augCol):
        m[rr][j] = m[rr][j] + cc*m[rrcol][j]
 
  # final reduction -- the first test catches under-determined systems
  # these are characterised by some equations being all zero
  for rb in reversed(range(eqns)):
    if ( m[rb][rb] == 0):
      if m[rb][augCol-1] == 0:
        continue
      else:
        print 'system is inconsistent'
        return
    else:
      # you must loop back across to catch under-determined systems
      for backCol in reversed(range(rb, augCol)):
        m[rb][backCol] = float(m[rb][backCol]) / float(m[rb][rb])
      # knock-up (cancel the above to eliminate the knowns)
      # again, we must loop to catch under-determined systems
      if not (rb == 0):
        for kup in reversed(range(rb)):
          for kleft in reversed(range(rb, augCol)):
            kk = -float(m[kup][rb]) / float(m[rb][rb])
            m[kup][kleft] += kk*float(m[rb][kleft])
 
  matxRound(m, decPts)
 
  if not b == False:
    return m
  else:
    mOut = []
    for row in range(len(m)):
      rOut = []
      for col in range(augCol/2, augCol):
        rOut.append(m[row][col])
      mOut.append(rOut)
    return mOut
 
# test it!
 
A =  [  [440444.44  ,     0.0000000 ,     -10666666 ,     -433333.31  ,     0.0000000   ,    0.0000000  ],[ 0.0000000  ,     436888.88  ,     5333333.0   ,    0.0000000    ,   4444.4443  ,     5333333.0   ],[-10666666.    ,   5333333.0   ,   3.19999980E+10 ,  0.0000000     , -5333333.0   ,   5.33333299E+09],[-433333.31    ,   0.0000000   ,    0.0000000     ,  440444.44     ,  0.0000000    ,   10666666.    ],[0.0000000     ,  4444.4443   ,   -5333333.0     ,  0.0000000    ,   436888.88     , -5333333.0    ],[0.0000000     ,  5333333.0   ,   5.33333299E+09 ,  10666666.    ,  -5333333.0    ,  3.19999980E+10]]

b = [40000.000  ,     0.0000000    ,   0.0000000     ,  0.0000000     ,  0.0000000     ,  500.00000   ]
 
sol = gj_Solve(A, b)
print '\n\n\n\n'
print 'sol is ', sol
 
inv = gj_Solve(A)
print 'inv is ', inv
