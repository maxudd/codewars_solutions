def det(matrix):
    n = len(matrix[0])
    return matrix[0][0] if n == 1 else sum([(-1)**(2+i)*matrix[0][i]*det([x[:i] + x[i+1:] for x in matrix[1:]]) for i in range(n)])
