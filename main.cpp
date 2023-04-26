//Rakhmetov Artur
//a.rakhmetov@innopolis.university
#include <iostream>

#include <vector>

#include <cstdio>

#include <stdlib.h>

#include <cmath>

#include <iomanip>


using namespace std;

int d = 1;
bool isZero(double num) {
  const double zero = 0.0000000001;
  if (abs(num) < zero) return true;
  return false;
}

class matrix {

  public:

    friend ostream & operator << (ostream & out,
      const matrix & m) {

      for (int i = 0; i < m.size_a; i++) {
        for (int j = 0; j < m.size_b; j++) {
          if (isZero(m.vec[i][j])) out << 0.00 << " ";
          else {
            out << m.vec[i][j] << " ";
          }

        }
        out << "\n";
      }
      out << m.error;
      return out;
    }

  friend istream & operator >> (istream & in, matrix & m) {
    for (int i = 0; i < m.size_a; i++) {
      for (int j = 0; j < m.size_b; j++) {
        in >> m.vec[i][j];
        if (isZero(m.vec[i][j])) m.vec[i][j] = 0;
      }
    }
    return in;
  }

  int size_a = 0;
  int size_b = 0;
  string error;
  vector < vector < double > > vec;
  matrix(int a, int b, vector < vector < double > > v) {
    vec.resize(a);
    for (int i = 0; i < a; i++) {
      vec[i].resize(b);
    }
    size_a = a;
    size_b = b;
    for (int i = 0; i < size_a; i++) {
      for (int j = 0; j < size_b; j++) {
        vec[i][j] = v[i][j];

      }
    }

  }

  matrix(int a, int b) {
    vec.resize(a);
    for (int i = 0; i < a; i++) {
      vec[i].resize(b);
    }
    size_a = a;
    size_b = b;
  }

  void exchange(int a, int b) {
    double c = 0;
    for (int i = 0; i < size_b; i++) {
      c = vec[a][i];
      vec[a][i] = vec[b][i];
      vec[b][i] = c;

    }
  }

  void operator = (matrix a) {
    for (int i = 0; i < a.size_a; i++) {
      for (int j = 0; j < a.size_b; j++) {
        vec[i][j] = a.vec[i][j];

      }
    }
  }

  matrix operator + (matrix b) {

    matrix nul(0, 0);
    if (size_a != b.size_a || size_b != b.size_b) {
      nul.error = "Error: the dimensional problem occurred\n";
      return nul;
    }
    matrix c(size_a, size_b);
    for (int i = 0; i < size_a; i++) {
      for (int j = 0; j < size_b; j++) {
        c.vec[i][j] = vec[i][j] + b.vec[i][j];
        if (isZero(c.vec[i][j])) c.vec[i][j] = 0;

      }

    }

    return c;
  }

  matrix operator - (matrix b) {

    matrix nul(0, 0);
    if (size_a != b.size_a || size_b != b.size_b) {
      nul.error = "Error: the dimensional problem occurred\n";
      return nul;
    }
    matrix c(size_a, size_b);
    for (int i = 0; i < size_a; i++) {
      for (int j = 0; j < size_b; j++) {
        c.vec[i][j] = vec[i][j] - b.vec[i][j];
        if (isZero(c.vec[i][j])) c.vec[i][j] = 0;
      }

    }

    return c;
  }

  matrix operator * (matrix b) {

    matrix nul(0, 0);
    if (size_b != b.size_a) {
      nul.error = "Error: the dimensional problem occurred\n";
      return nul;
    }
    matrix c(size_a, b.size_b);
    for (int i = 0; i < size_a; i++) {
      for (int j = 0; j < b.size_b; j++) {
        for (int n = 0; n < size_b; n++) {

          c.vec[i][j] += vec[i][n] * b.vec[n][j];
          if (isZero(c.vec[i][j])) c.vec[i][j] = 0;
        }
      }

    }

    return c;
  }

  matrix transpose() {
    matrix c(size_b, size_a);
    for (int i = 0; i < size_a; i++) {
      for (int j = 0; j < size_b; j++) {
        c.vec[j][i] = vec[i][j];

      }
    }

    return c;
  }

};

class ColumnVector: public matrix {

  public: ColumnVector(int a): matrix(a, 1) {

  }
  public: ColumnVector(int a, vector < vector < double > > v): matrix(a, 1, v) {

  }

  void operator = (matrix A) {
    matrix::operator = (A);

  }

};

class identityMatrix: public matrix {

  public:

    identityMatrix(int a): matrix(a, a) {

      for (int i = 0; i < a; i++) {
        vec[i][i] = 1;

      }

    }

};

class eleminationMatrix: public identityMatrix {

  public:

    eleminationMatrix(matrix A, int a, int b): identityMatrix(A.size_a) {

      vec[a][b] = A.vec[a][b] / A.vec[b][b] * -1;

    }

  void operator = (matrix A) {
    matrix::operator = (A);

  }

};

class permutationMatrix: public identityMatrix {

  public:

    permutationMatrix(int n, int a, int b): identityMatrix(n) {
      vec[a][a] = 0;
      vec[b][b] = 0;
      vec[a][b] = 1;
      vec[b][a] = 1;

    }

};

class squareMatrix: public matrix {

  public:

    squareMatrix(int a, vector < vector < double > > v): matrix(a, a, v) {

    }

  squareMatrix(int a): matrix(a, a) {

  }

  void operator = (matrix A) {
    matrix::operator = (A);

  }

  int FindDiag(int num) {
    int diag = num;
    double value = vec[num][num];
    for (int i = num + 1; i < size_a; i++) {
      if (abs(vec[i][num]) > abs(value)) {
        value = vec[i][num];
        diag = i;
      }
    }

    return diag;
  }

  void REF(matrix & C) {

    for (int i = 0; i < size_a; i++) {
      int biggest = FindDiag(i);
      if (biggest != i) {

        d += 1;
        permutationMatrix P(size_a, i, biggest);
        * this = P ** this;
        C = P * C;

      }
      for (int j = i + 1; j < size_a; j++) {
        eleminationMatrix A( * this, j, i);
        if (!isZero(vec[j][i])) {

          d += 1;
          * this = A ** this;
          C = A * C;

        }
      }

    }

  }

  void RREF(matrix & C) {

    for (int i = size_a - 1; i >= 0; i--) {
      for (int j = i - 1; j >= 0; j--) {
        eleminationMatrix A( * this, j, i);
        if (!isZero(vec[j][i])) {

          d += 1;
          * this = A ** this;
          C = A * C;

        }
      }

    }

    double del;
    for (int i = 0; i < size_a; i++) {
      del = normalizeRow(i);
      for (int j = 0; j < size_a; j++) {
        if (!isZero(del)) {
          C.vec[i][j] /= del;
        }
      }
    }

  }

  double normalizeRow(int diag) {
    if (isZero(vec[diag][diag])) {
      return 0;
    }
    double del = vec[diag][diag];
    for (int i = 0; i < size_b; i++) {
      vec[diag][i] = vec[diag][i] / del;
      if (isZero(vec[diag][i])) vec[diag][i] = 0;

    }

    return del;
  }
  double determinant() {
    double result = 1;
    for (int i = 0; i < size_a; i++) {
      result *= vec[i][i];

    }
    return result;
  }

  squareMatrix transpose() {
    squareMatrix result = * this;
    result = result.matrix::transpose();
    return result;

  }

};

class augmentedMatrix: public squareMatrix {

  public: friend istream & operator >> (istream & in, squareMatrix & m) {
    for (int i = 0; i < m.size_a; i++) {
      for (int j = 0; j < m.size_a; j++) {
        in >> m.vec[i][j];
        if (isZero(m.vec[i][j])) m.vec[i][j] = 0;
      }
      for (int j = 0; j < m.size_a; j++) {
        if (j == i) m.vec[i].push_back(1);
        else m.vec[i].push_back(0);
      }
    }
    return in;
  }

  augmentedMatrix(int a): squareMatrix(a) {

    size_b += size_a;
  }

};

identityMatrix inverse(squareMatrix A) {
  identityMatrix I(A.size_a);
  A.REF(I);
  A.RREF(I);
  return I;
}

int main() {
  cout << fixed << setprecision(4);
  int n;
  cin >> n;
  vector < vector < double > > b(n);
  vector < vector < double > > vec;
  vector < double > input(n);
  vec.resize(n);
  b.resize(n);
  double ai, bi;
  for (int i = 0; i < n; i++) {
    b[i].resize(1);
    cin >> ai >> bi;
    input[i] = ai;
    b[i][0] = bi;
  }
  int dem;
  cin >> dem;
  for (int i = 0; i < n; i++) {
    vec[i].resize(dem + 1);
    for (int j = 0; j < dem + 1; j++) {
      vec[i][j] = pow(input[i], j);
    }
  }

  matrix A(n, dem + 1, vec);
  ColumnVector C(n, b);
  cout << "A:\n";
  cout << A;
  matrix A_T = A.transpose();
  cout << "A_T*A:\n";
  squareMatrix AT_A(dem + 1);
  AT_A = A_T * A;
  cout << AT_A;
  AT_A = inverse(AT_A);

  cout << "(A_T*A)^-1:\n";
  cout << AT_A;
  cout << "A_T*b:\n";
  cout << A_T * C;
  cout << "x~:\n";
  cout << AT_A * A_T * C;
  return 0;
}
