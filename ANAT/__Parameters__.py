# the parameter configuration file for adjoint tomography

# =====================================================================================
WorkDir = '/work/ose-zhaoy/packageForANAT/ANAT'             # work directory
ProcDir = '/scratch/2022-05-10/ose-zhaoy/ANAT'              # process directory, heavy computation with large disk space
BinDir = '/work/ose-zhaoy/specfem3d/bin'                    # source program directory
DataDir = WorkDir + '/data'                                 # data directory
ToolsDir = WorkDir + '/tools'                               # adjoint tools directory
ScriptsDir = WorkDir + '/scripts'                           # some scripts may be useful for user
ResultsDir = ProcDir + '/results'                           # results directory
# =====================================================================================
# parameters should be changed over iterations
mod = 'M00'             # current model
step = '0.01'           # step length, if mod='M00' step is not used

cc = [0.70, 0.70, 0.70]
# cc = [0.75, 0.75, 0.75]
# cc = [0.80, 0.80, 0.80]
# cc = [0.85, 0.85, 0.85]
                        # cross-correlation coefficients

tshift = [5.0, 5.0, 5.0]
# tshift = [4.0, 4.0, 4.0]
# tshift = [3.0, 3.0, 3.0]
# tshift = [2.0, 2.0, 2.0]
                        # tshift: maximum allowed time shift in cc measurements

step_range = '0.02 0.02 0.12'
# step_range = '0.01 0.02 0.09'
# step_range = '0.01 0.01 0.05'
                        # step_range: step range for the line search

hsmooth, vsmooth = 20000, 15000
# hsmooth, vsmooth = 15000, 10000
# hsmooth, vsmooth = 10000, 7000
                        # horizontal and vertical smooth length
# =====================================================================================
mpi_version = 'mpi/openmpi/3.1.2_gcc'
sac_path = '/work/ose-zhaoy/software/sac/aux'
zone = 48               # UTM zone of the study region
bands = ['T005_T010', 'T010_T020', 'T020_T050']
                        # split waveforms to three period bands
nproc = 36              # number of processors for parallel computing
# =====================================================================================
num_set = 6             # split dataset to num_set sets
num_station = 5         # number of stations in each set
# =====================================================================================
dt = 0.04               # time interval for the forward simulation (maximum value is
                        # constrained by the meshing procedure)

nstep = 9000            # number of time steps for the forward simulation (minimum value
                        # should be determined by plotting the syntheics of the station
                        # pair with maximum spacing.)


nstep_between_output_info = 1000
                        # interval for the time step info (nstep should be exactly divisible
                        # by nstep_between_output_info to check the result of the last simulation step)

df = 100                # resampling rate
tstart = -2.0           # start time of the simulation (e.g. -2.0)
tspan = '-20/370'       # time span used to cut the synthetic and real data
                        # t1 should be smaller than or equal to tstart (e.g. -2.0)
                        # t2: should be larger than dt*nstep

channel = 'BX'          # channel name, which can be obtained from a forward computation
                        # different data set has different chanel, e.g. HX, CX etc.

Vgroup = [(1.7, 3.6), (2.2, 3.7), (2.6, 4.3)]
                        # default group velocity for window size selection in measurement.
