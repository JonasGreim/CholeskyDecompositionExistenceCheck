# Cholesky decomposition existence check

<!-- ABOUT THE PROJECT -->

## About The Project
I wrote this Fortran code for a university project. It is clearly not the best fortran code out there, 
but the code works with every quadratic matrices. You can test whether there is Cholesky decomposition or not.
I am uploading this project because if someone gets a similar task in the following semester, this would help and I have not found a standard solution for this problem via google.


Usually you don't comment every line of code, but it was requested that way. I leave the comments like this because
 I think it helps a lot if you don´t understand Fortran well.




## Installation
 - Install at least Fortran 90
 - Go to the root folder of the project  
 - Compile (Windows)
  ```sh   
   gfortran code.f90
  ```
 - Execute (Windows)
 ```sh   
   a.exe
  ```

## How to use it

 * Change this variable to the length of your quadratic matrix (Line 3)
   ```sh  
   integer , parameter:: columnRowLength = 4
     ```
 * Put your matrix formatted in .txt or .csv in the root folder of the project and change it to your file name (line 15)
   ```sh  
   open(10,file='CholeskyPossible.txt')
    ```
   For test purposes you can use the matrices: CholeskyNotPossible.txt, CholeskyPossible.txt & 5x5Matrix.txt (last one you have to change the variable to 5) 
 * Compile and run/execute the project
 * The results are now printed out in your command line
## How it works
If the matrix is quadratic, symmetric and positiv definit => then there is a Cholesky decomposition.

Test symmetry: 
 - compare the matrix to its own transposition

Test positiv definit:
 * via Laplace expansion: 
   * If Laplace expansion is greater than 0 then the matrix is semi postiv definit
   * And if Laplace expansion is is greater than 0 and the matrix is symmetric then the matrix is postiv definit
 

<!-- LICENSE -->

## License

Distributed under the MIT License. See `LICENSE` for more information.
