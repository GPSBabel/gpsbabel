/*
    Copyright (C) 2025 Robert Lipe, gpsbabel.org

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
*/

#include "matrix.h"
#include "defs.h" // For gbFatal
#include <cmath> // For std::abs
#include <algorithm> // For std::swap

Matrix::Matrix(int rows, int cols) : rows_(rows), cols_(cols), data_(rows * cols, 0.0) {}

double& Matrix::operator()(int row, int col) {
    return data_[row * cols_ + col];
}

const double& Matrix::operator()(int row, int col) const {
    return data_[row * cols_ + col];
}

Matrix Matrix::identity(int size) {
    Matrix result(size, size);
    for (int i = 0; i < size; ++i) {
        result(i, i) = 1.0;
    }
    return result;
}

Matrix Matrix::transpose() const {
    Matrix result(cols_, rows_);
    for (int i = 0; i < rows_; ++i) {
        for (int j = 0; j < cols_; ++j) {
            result(j, i) = (*this)(i, j);
        }
    }
    return result;
}

Matrix Matrix::inverse() const {
    if (rows_ != cols_) {
        gbFatal("Matrix inverse is only defined for square matrices.");
    }

    int n = rows_;
    Matrix augmented_matrix(n, 2 * n);

    // Initialize augmented matrix [A|I]
    for (int i = 0; i < n; ++i) {
        for (int j = 0; j < n; ++j) {
            augmented_matrix(i, j) = (*this)(i, j);
        }
        augmented_matrix(i, i + n) = 1.0; // Identity matrix on the right
    }

    // Gaussian elimination
    for (int i = 0; i < n; ++i) {
        // Find pivot
        int pivot_row = i;
        for (int k = i + 1; k < n; ++k) {
            if (std::abs(augmented_matrix(k, i)) > std::abs(augmented_matrix(pivot_row, i))) {
                pivot_row = k;
            }
        }

        // Swap rows if necessary
        if (pivot_row != i) {
            for (int j = 0; j < 2 * n; ++j) {
                std::swap(augmented_matrix(i, j), augmented_matrix(pivot_row, j));
            }
        }

        double pivot = augmented_matrix(i, i);
        if (std::abs(pivot) < 1e-9) { // Check for singular matrix
            gbFatal("Matrix is singular and cannot be inverted.");
        }

        // Divide row by pivot to make diagonal element 1
        for (int j = i; j < 2 * n; ++j) {
            augmented_matrix(i, j) /= pivot;
        }

        // Eliminate other rows
        for (int row = 0; row < n; ++row) {
            if (row != i) {
                double factor = augmented_matrix(row, i);
                for (int j = i; j < 2 * n; ++j) {
                    augmented_matrix(row, j) -= factor * augmented_matrix(i, j);
                }
            }
        }
    }

    // Extract the inverse matrix
    Matrix result(n, n);
    for (int i = 0; i < n; ++i) {
        for (int j = 0; j < n; ++j) {
            result(i, j) = augmented_matrix(i, j + n);
        }
    }
    return result;
}

Matrix Matrix::operator+(const Matrix& other) const {
    if (rows_ != other.rows_ || cols_ != other.cols_) {
        gbFatal("Matrix dimensions must match for addition.");
    }
    Matrix result(rows_, cols_);
    for (unsigned int i = 0; i < data_.size(); ++i) {
        result.data_[i] = data_[i] + other.data_[i];
    }
    return result;
}

Matrix Matrix::operator-(const Matrix& other) const {
    if (rows_ != other.rows_ || cols_ != other.cols_) {
        gbFatal("Matrix dimensions must match for subtraction.");
    }
    Matrix result(rows_, cols_);
    for (unsigned int i = 0; i < data_.size(); ++i) {
        result.data_[i] = data_[i] - other.data_[i];
    }
    return result;
}

Matrix Matrix::operator*(const Matrix& other) const {
    if (cols_ != other.rows_) {
        gbFatal("Matrix dimensions are not compatible for multiplication.");
    }
    Matrix result(rows_, other.cols_);
    for (int i = 0; i < rows_; ++i) {
        for (int j = 0; j < other.cols_; ++j) {
            for (int k = 0; k < cols_; ++k) {
                result(i, j) += (*this)(i, k) * other(k, j);
            }
        }
    }
    return result;
}

Matrix Matrix::operator*(double scalar) const {
    Matrix result(rows_, cols_);
    for (unsigned int i = 0; i < data_.size(); ++i) {
        result.data_[i] = data_[i] * scalar;
    }
    return result;
}
