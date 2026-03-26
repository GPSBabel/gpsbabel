#ifndef MATRIX_H
#define MATRIX_H

#include <vector>  // for vector

#include <QDebug>  // for QDebug

class Matrix {
public:
    Matrix(int rows, int cols);


    double& operator()(int row, int col);
    const double& operator()(int row, int col) const;

    int rows() const { return rows_; }
    int cols() const { return cols_; }


    static Matrix identity(int size);
    Matrix transpose() const;
    Matrix inverse() const;

    Matrix operator+(const Matrix& other) const;
    Matrix operator-(const Matrix& other) const;
    Matrix operator*(const Matrix& other) const;
    Matrix operator*(double scalar) const;

    Matrix& operator+=(const Matrix& other);
    Matrix& operator-=(const Matrix& other);
    Matrix& operator*=(double scalar);

private:
    int rows_;
    int cols_;
    std::vector<double> data_;
};

Matrix operator*(double scalar, const Matrix& matrix);
QDebug operator<<(QDebug debug, const Matrix& m);

#endif // MATRIX_H
