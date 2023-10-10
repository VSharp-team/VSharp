// C# Program to count islands in boolean 2D matrix
using System;
using System.Collections.Generic;
using NUnit.Framework;
using VSharp.Test;

[TestSvmFixture, Category("Dataset")]
class Islands {

    // A utility function to do DFS for a 2D
    // boolean matrix. It only considers
    // the 8 neighbours as adjacent vertices
    static void DFS(int[, ] M, int i, int j, int ROW,
        int COL)
    {

        // Base condition
        // if i less than 0 or j less than 0 or i greater
        // than ROW-1 or j greater than COL- or if M[i][j]
        // != 1 then we will simply return
        if (i < 0 || j < 0 || i > (ROW - 1) || j > (COL - 1)
            || M[i, j] != 1) {
            return;
        }

        if (M[i, j] == 1) {
            M[i, j] = 0;
            DFS(M, i + 1, j, ROW,
                COL); // right side traversal
            DFS(M, i - 1, j, ROW,
                COL); // left side traversal
            DFS(M, i, j + 1, ROW,
                COL); // upward side traversal
            DFS(M, i, j - 1, ROW,
                COL); // downward side traversal
            DFS(M, i + 1, j + 1, ROW,
                COL); // upward-right side traversal
            DFS(M, i - 1, j - 1, ROW,
                COL); // downward-left side traversal
            DFS(M, i + 1, j - 1, ROW,
                COL); // downward-right side traversal
            DFS(M, i - 1, j + 1, ROW,
                COL); // upward-left side traversal
        }
    }

    [TestSvm(50,serialize:"countIslands"), Category("Dataset")]
    public static int countIslands(int[, ] M)
    {
        int ROW = M.GetLength(0);
        int COL = M.GetLength(1);
        int count = 0;
        for (int i = 0; i < ROW; i++) {
            for (int j = 0; j < COL; j++) {
                if (M[i, j] == 1) {
                    count++;
                    DFS(M, i, j, ROW,
                        COL); // traversal starts from
                    // current cell
                }
            }
        }
        return count;
    }

    // Driver code
    static int IslandsMain(int[,] M)
    {
        
        return countIslands(M);
    }
}
