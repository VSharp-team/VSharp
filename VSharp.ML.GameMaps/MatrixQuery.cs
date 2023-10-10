// C# Code For A Boolean
// Matrix Question

using NUnit.Framework;
using VSharp.Test;

[TestSvmFixture, Category("Dataset")]
class MatrixQuery {
    [TestSvm(50,serialize:"MatrixQueryModifyMatrix"), Category("Dataset")]
    public static void MatrixQueryModifyMatrix(int[, ] mat)
    {

        // variables to check
        // if there are any 1
        // in first row and column
        bool row_flag = false;
        bool col_flag = false;

        // updating the first
        // row and col if 1
        // is encountered
        for (int i = 0; i < mat.GetLength(0); i++) {
            for (int j = 0; j < mat.GetLength(1); j++) {
                if (i == 0 && mat[i, j] == 1)
                    row_flag = true;

                if (j == 0 && mat[i, j] == 1)
                    col_flag = true;

                if (mat[i, j] == 1) {
                    mat[0, j] = 1;
                    mat[i, 0] = 1;
                }
            }
        }

        // Modify the input matrix mat[]
        // using the first row and first
        // column of Matrix mat
        for (int i = 1; i < mat.GetLength(0); i++) {
            for (int j = 1; j < mat.GetLength(1); j++) {

                if (mat[0, j] == 1 || mat[i, 0] == 1) {
                    mat[i, j] = 1;
                }
            }
        }

        // modify first row
        // if there was any 1
        if (row_flag == true) {
            for (int i = 0; i < mat.GetLength(1); i++) {
                mat[0, i] = 1;
            }
        }

        // modify first col if
        // there was any 1
        if (col_flag == true) {
            for (int i = 0; i < mat.GetLength(0); i++) {
                mat[i, 0] = 1;
            }
        }
    }
    
    public static int[,] MatrixQueryMain(int[,] mat)
    {
        MatrixQueryModifyMatrix(mat);
        return mat;
    }
}
