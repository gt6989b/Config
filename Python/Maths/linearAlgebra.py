#!/usr/bin/python

##----------------------------------------------------------------------------##
## \file  linearAlgebra.py                                                    ##
## \brief Linear algebra utilities                                            ##
##                                                                            ##
## $URL: https://trstwprsvnapv01.treasury.corp.ge.com/svn/Treasury/QUANTS%20GROUP/Branches/Dima/mdm0/Utils/Python/Maths/linearAlgebra.py $
## $Id: linearAlgebra.py 11844 2014-08-06 20:27:31Z 212361829 $               ##
##----------------------------------------------------------------------------##

import numpy as np

def naivePCA(matrix, order = 0, signs = []):
    """@brief Decompose @matrix into principal components and characterize them

    This basically performs singular value decomposition of the covariance
    matrix (A^T A)/n, where A is an n x k matrix with n>k. The eigenvectors
    will coincide with A exactly, and the eigenvalues will be the squares of
    the eigenvalues of A.

    The output sort order by eigenvalue size can be set using the @order
    parameter: +1/-1 is increasing/decreasing, others mean do not sort.

    When computing the eigenvalues, any sign can be chosen. We set the signs of
    the first coordinates of the eigenvalues to the @signs parameter. If it is
    omitted, no sign convention is guaranteed.

    :param matrix:  2D np.array to do PCA on.
    :param signs:   A list of signs for eigenvalue's first coordinate to match
    :return: eigenvalues as 2D np.array, eigenvectors as 1D np.array
    """

    numRows, numCols = matrix.shape

    # The real covariance matrix is (A^T A)/numRows, but since we only want the
    # eigen-decomposition, we don't have to divide every entry by numRows.
    # Instead, we will just rescale the eigenvalues.

    # diagonalize A^T A
    # since A is real-valued, A^T A is positive semi-definite so it is
    # diagonalizable, and the resulting eigenvalues will be non-negative
    eigenValues, eigenVectors = np.linalg.eig(np.dot(matrix.T, matrix))

    if order in [1,-1]:
        eigenSortIdx = np.argsort(eigenValues)
        if order == -1:
            eigenSortIdx = eigenSortIdx[::-1]
        eigenValues  = eigenValues[eigenSortIdx]
        eigenVectors = eigenVectors[:, eigenSortIdx]

    if len(signs) > 0:
        if len(signs) == numCols:
            eigenVectors[:,eigenVectors[0]*signs < 0] *= -1.
        else:
            raise ValueError('len(signs) = %d != number of columns (%d)' \
                                 % (len(signs), numCols))

    return np.sqrt(eigenValues/numRows), eigenVectors
