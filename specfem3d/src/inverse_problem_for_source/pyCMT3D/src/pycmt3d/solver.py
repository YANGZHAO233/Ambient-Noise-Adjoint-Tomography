#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""
Solver functions

:copyright:
    Wenjie Lei (lei@Princeton.EDU), 2015
:license:
    GNU Lesser General Public License, Version 3
    (http://www.gnu.org/copyleft/lgpl.html)
"""
from __future__ import print_function, division, absolute_import
import numpy as np
from .measure import get_f_df
from . import logger


def linear_solver(old_par, A, b, npar, zero_trace=True):
    """
    if invert for moment tensor with zero-trace constraints
    or no constraint
    """
    if zero_trace:
        na = npar + 1
    else:
        na = npar

    AA = np.zeros([na, na])
    bb = np.zeros(na)
    AA[0:npar, 0:npar] = A
    bb[0:npar] = b

    if zero_trace:
        bb[na - 1] = - np.sum(old_par[0:3])
        AA[0:6, na - 1] = np.array([1, 1, 1, 0, 0, 0])
        AA[na - 1, 0:6] = np.array([1, 1, 1, 0, 0, 0])
        AA[na - 1, na - 1] = 0.0

    try:
        dm = np.linalg.solve(AA, bb)
    except Exception as err:
        raise ValueError("Can not solve the linear equation due to:%s"
                         % err)
    # logger.info("Residual norm: %f" % (np.linalg.norm(bb - AA * dm)))
    new_par = old_par[0:npar] + dm[0:npar]
    return new_par


def nonlinear_solver(old_par, A, b, npar, max_iter=60):
    """
    if invert for moment tensor with double couple constraints
    setup starting solution, solve directly for moment instead
    of dm, exact implementation of (A16)
    logger.info('Non-linear Inversion')

    :return:
    """
    na = npar + 2
    mstart = np.copy(old_par)
    m1 = np.copy(mstart)
    lam = np.zeros(2)
    AA = np.zeros([na, na])
    bb = np.zeros(na)

    error = np.zeros([max_iter, na])
    for iter_idx in range(max_iter):
        get_f_df(npar, A, b, m1, lam, mstart, AA, bb)
        bb = - bb
        xout = np.linalg.solve(AA, bb)
        if np.isclose(np.linalg.norm(xout), 0):
            break
        m1 = m1 + xout[0:npar]
        lam = lam + xout[npar:na]
        error[iter_idx, :] = np.dot(AA, xout) - bb
    # logger.info("Nonlinear iteration: %d" % iter_idx)
    # logger.info("Residual norm: %f" % (np.linalg.norm(bb - AA * xout)))
    return m1


def solver(npar, A, b, cmt_par, zero_trace, double_couple,
           damping, max_nl_iter, verbose=True):
    """
    Solver part. Hession matrix A and misfit vector b will be
    reconstructed here based on different constraints.

    :param A: basic Hessian matrix
    :param b: basic misfit vector
    :param print_mode: if True, then print out log information;
    if False, then no log information
    :return:
    """
    A = A.copy()
    b = b.copy()

    # scale the A and b matrix by the max value
    # not really necessary, should be deleted in the future
    max_row = np.amax(abs(A), axis=1)
    for i in range(len(b)):
        A[i, :] /= max_row[i]
        b[i] /= max_row[i]

    # add damping
    if verbose:
        logger.info("Condition number of A: %10.2f"
                    % np.linalg.cond(A))
    if damping > 0:
        trace = np.matrix.trace(A)
        damp_matrix = np.zeros([npar, npar])
        np.fill_diagonal(damp_matrix, trace * damping)
        A = A + damp_matrix
        if verbose:
            logger.info("Condition number of A after damping: %10.2f"
                        % np.linalg.cond(A))

    # setup inversion schema
    if double_couple:
        # non-linear inversion
        if verbose:
            logger.info("Nonlinear Inversion...")
        new_par = nonlinear_solver(
            cmt_par, A, b, npar,
            max_iter=max_nl_iter)
    else:
        # linear_inversion
        if verbose:
            logger.info("Linear Inversion...")
        new_par = linear_solver(
            cmt_par, A, b, npar, zero_trace=zero_trace)

    return new_par
