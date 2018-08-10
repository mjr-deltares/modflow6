import os
import sys
import numpy as np

try:
    import pymake
except:
    msg = 'Error. Pymake package is not available.\n'
    msg += 'Try installing using the following command:\n'
    msg += ' pip install https://github.com/modflowpy/pymake/zipball/master'
    raise Exception(msg)

try:
    import flopy
except:
    msg = 'Error. FloPy package is not available.\n'
    msg += 'Try installing using the following command:\n'
    msg += ' pip install flopy'
    raise Exception(msg)

from framework import testing_framework
from simulation import Simulation

ex = ['csub_subwt01a', 'csub_subwt01b', 'csub_subwt01c']
exdirs = []
for s in ex:
    exdirs.append(os.path.join('temp', s))
ddir = 'data'
cmppth = 'mfnwt'

htol = [None, None, 0.075]
dtol = 1e-3
budtol = 1e-2

paktest = 'csub'

ump = ['None', True, 'None']
ivoid = [0, 1, 0]
gs0 = [0., 0., 1700.]

# set travis to True when version 1.13.0 is released
travis = [True, True]

# set replace_exe to None to use default executable
replace_exe = None

# static model data
pth = os.path.join(ddir, 'ibc01_ibound.ref')
ib0 = np.genfromtxt(pth)

# temporal discretization
nper = 3
perlen = [1., 21915., 21915.]
nstp = [1, 60, 60]
tsmult = [1., 1., 1.]
steady = [True, False, False]

# spatial discretization
nlay, nrow, ncol = 4, ib0.shape[0], ib0.shape[1]
shape3d = (nlay, nrow, ncol)
size3d = nlay * nrow * ncol
nactive = np.count_nonzero(ib0) * nlay

delr, delc = 2000., 2000.
top = 150.
botm = [50., -100., -150., -350.]
strt = 100.

hnoflo = 1e30
hdry = -1e30

# upw data
laytyp = [1, 0, 0, 0]
hk = [4., 4., 1e-2, 4.]
sy = [0.3, 0., 0., 0.]

w1 = [(0, 0, 7, 2.2000000E+03),
      (0, 1, 4, 2.2000000E+03),
      (0, 1, 7, 2.2000000E+03),
      (0, 1, 11, 2.2000000E+03),
      (0, 2, 3, 2.2000000E+03),
      (0, 3, 11, 2.2000000E+03),
      (0, 4, 2, 2.2000000E+03),
      (0, 4, 12, 2.2000000E+03),
      (0, 5, 13, 2.2000000E+03),
      (0, 6, 1, 2.2000000E+03),
      (0, 13, 1, 2.2000000E+03),
      (0, 13, 13, 2.2000000E+03),
      (0, 15, 2, 2.2000000E+03),
      (0, 15, 12, 2.2000000E+03),
      (0, 16, 12, 2.2000000E+03),
      (0, 17, 3, 2.2000000E+03),
      (0, 17, 11, 2.2000000E+03),
      (0, 18, 6, 2.2000000E+03)]
w2 = [(0, 0, 7, 2.2000000E+03),
      (0, 1, 4, 2.2000000E+03),
      (0, 1, 7, 2.2000000E+03),
      (0, 1, 11, 2.2000000E+03),
      (0, 2, 3, 2.2000000E+03),
      (0, 3, 11, 2.2000000E+03),
      (0, 4, 2, 2.2000000E+03),
      (0, 4, 12, 2.2000000E+03),
      (0, 5, 13, 2.2000000E+03),
      (0, 6, 1, 2.2000000E+03),
      (0, 13, 1, 2.2000000E+03),
      (0, 13, 13, 2.2000000E+03),
      (0, 15, 2, 2.2000000E+03),
      (0, 15, 12, 2.2000000E+03),
      (0, 16, 12, 2.2000000E+03),
      (0, 17, 3, 2.2000000E+03),
      (0, 17, 11, 2.2000000E+03),
      (0, 18, 6, 2.2000000E+03),
      (1, 8, 9, -7.2000000E+04),
      (3, 11, 6, -7.2000000E+04)]
wd = {0: w1, 1: w2, 2: w1}

ws1 = [((0, 0, 7), 2.2000000E+03),
       ((0, 1, 4), 2.2000000E+03),
       ((0, 1, 7), 2.2000000E+03),
       ((0, 1, 11), 2.2000000E+03),
       ((0, 2, 3), 2.2000000E+03),
       ((0, 3, 11), 2.2000000E+03),
       ((0, 4, 2), 2.2000000E+03),
       ((0, 4, 12), 2.2000000E+03),
       ((0, 5, 13), 2.2000000E+03),
       ((0, 6, 1), 2.2000000E+03),
       ((0, 13, 1), 2.2000000E+03),
       ((0, 13, 13), 2.2000000E+03),
       ((0, 15, 2), 2.2000000E+03),
       ((0, 15, 12), 2.2000000E+03),
       ((0, 16, 12), 2.2000000E+03),
       ((0, 17, 3), 2.2000000E+03),
       ((0, 17, 11), 2.2000000E+03),
       ((0, 18, 6), 2.2000000E+03)]
ws2 = [((0, 0, 7), 2.2000000E+03),
       ((0, 1, 4), 2.2000000E+03),
       ((0, 1, 7), 2.2000000E+03),
       ((0, 1, 11), 2.2000000E+03),
       ((0, 2, 3), 2.2000000E+03),
       ((0, 3, 11), 2.2000000E+03),
       ((0, 4, 2), 2.2000000E+03),
       ((0, 4, 12), 2.2000000E+03),
       ((0, 5, 13), 2.2000000E+03),
       ((0, 6, 1), 2.2000000E+03),
       ((0, 13, 1), 2.2000000E+03),
       ((0, 13, 13), 2.2000000E+03),
       ((0, 15, 2), 2.2000000E+03),
       ((0, 15, 12), 2.2000000E+03),
       ((0, 16, 12), 2.2000000E+03),
       ((0, 17, 3), 2.2000000E+03),
       ((0, 17, 11), 2.2000000E+03),
       ((0, 18, 6), 2.2000000E+03),
       ((1, 8, 9), -7.2000000E+04),
       ((3, 11, 6), -7.2000000E+04)]
wd6 = {0: ws1, 1: ws2, 2: ws1}

chd1 = [(0, 19, 7, 100.00000, 100.00000),
        (0, 19, 8, 100.00000, 100.00000),
        (1, 19, 7, 100.00000, 100.00000),
        (1, 19, 8, 100.00000, 100.00000),
        (2, 19, 7, 100.00000, 100.00000),
        (2, 19, 8, 100.00000, 100.00000),
        (3, 19, 7, 100.00000, 100.00000),
        (3, 19, 8, 100.00000, 100.00000)]
cd = {0: chd1}

chd6 = [((0, 19, 7), 100.00000),
        ((0, 19, 8), 100.00000),
        ((1, 19, 7), 100.00000),
        ((1, 19, 8), 100.00000),
        ((2, 19, 7), 100.00000),
        ((2, 19, 8), 100.00000),
        ((3, 19, 7), 100.00000),
        ((3, 19, 8), 100.00000)]
cd6 = {0: chd6}

nouter, ninner = 100, 300
hclose, rclose, relax = 1e-6, 0.01, 0.97
fluxtol = nactive * rclose

tdis_rc = []
for idx in range(nper):
    tdis_rc.append((perlen[idx], nstp[idx], tsmult[idx]))

# this used to work
# ib = np.zeros((nlay, nrow, ncol), dtype=np.int)
# for k in range(nlay):
#    ib[k, :, :] = ib0.copy()
ib = []
for k in range(nlay):
    ib.append(ib0.astype(np.int).copy())

# subwt data
cc = 0.25
cr = 0.01
void = 0.82
theta = void / (1. + void)
kv = 999.
sgm = 1.7
sgs = 2.0
ini_stress = 15.0
delay_flag = 0
thick = [45., 70., 50., 90.]

zthick = [top - botm[0],
          botm[0] - botm[1],
          botm[1] - botm[2],
          botm[2] - botm[3]]

beta = 0.
#beta = 4.65120000e-10
gammaw = 9806.65000000
sw = beta * gammaw * theta
ss = [sw for k in range(nlay)]

swt6 = []
ibcno = 0
for k in range(nlay):
    for i in range(nrow):
        for j in range(ncol):
            iactive = 0
            if ib0[i, j] > 0:
                iactive = 1
            if i == 19 and (j == 7 or j == 8):
                iactive = 0
            if iactive > 0:
                ibcno += 1
                d = [ibcno, (k, i, j), 'nodelay', ini_stress, thick[k],
                     1., cc, cr, theta,
                     kv, 999.]
                swt6.append(d)
ds16 = [0, 0, 0, 2052, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
ds17 = [0, 10000, 0, 10000, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]


def get_model(idx, dir):
    name = ex[idx]

    # build MODFLOW 6 files
    ws = dir
    sim = flopy.mf6.MFSimulation(sim_name=name, version='mf6',
                                 exe_name='mf6',
                                 sim_ws=ws)
    # create tdis package
    tdis = flopy.mf6.ModflowTdis(sim, time_units='DAYS',
                                 nper=nper, perioddata=tdis_rc)

    # create gwf model
    gwf = flopy.mf6.ModflowGwf(sim, modelname=name, save_flows=True,
                               newtonoptions='')

    # create iterative model solution and register the gwf model with it
    ims = flopy.mf6.ModflowIms(sim, print_option='SUMMARY',
                               outer_hclose=hclose,
                               outer_maximum=nouter,
                               under_relaxation='NONE',
                               inner_maximum=ninner,
                               inner_hclose=hclose, rcloserecord=rclose,
                               linear_acceleration='BICGSTAB',
                               scaling_method='NONE',
                               reordering_method='NONE',
                               relaxation_factor=relax)
    sim.register_ims_package(ims, [gwf.name])

    dis = flopy.mf6.ModflowGwfdis(gwf, nlay=nlay, nrow=nrow, ncol=ncol,
                                  delr=delr, delc=delc,
                                  top=top, botm=botm,
                                  idomain=ib,
                                  fname='{}.dis'.format(name))

    # initial conditions
    ic = flopy.mf6.ModflowGwfic(gwf, strt=strt,
                                fname='{}.ic'.format(name))

    # node property flow
    npf = flopy.mf6.ModflowGwfnpf(gwf, save_flows=False,
                                  icelltype=laytyp,
                                  k=hk,
                                  k33=hk)
    # storage
    sto = flopy.mf6.ModflowGwfsto(gwf, save_flows=False, iconvert=laytyp,
                                  ss=0., sy=sy,
                                  steady_state={0: True},
                                  transient={1: True})

    # chd files
    chd = flopy.mf6.modflow.mfgwfchd.ModflowGwfchd(gwf,
                                                   maxbound=len(chd6),
                                                   stress_period_data=cd6,
                                                   save_flows=False)

    # wel files
    wel = flopy.mf6.ModflowGwfwel(gwf, print_input=True, print_flows=True,
                                  maxbound=len(ws2),
                                  stress_period_data=wd6,
                                  save_flows=False)

    # csub files
    sig0 = {0: [gs0[idx] for k in range(nlay)]}
    opth = '{}.csub.obs'.format(name)
    csub = flopy.mf6.ModflowGwfcsub(gwf,
                                    #interbed_stress_offset=True,
                                    compression_indices=True,
                                    geo_stress_offset=True,
                                    update_material_properties=ump[idx],
                                    time_weight=0.,
                                    ninterbeds=len(swt6),
                                    obs_filerecord=opth,
                                    sgs=sgs, sgm=sgm,
                                    beta=beta,
                                    gammaw=gammaw,
                                    ske_cr=0.,
                                    sk_theta=theta,
                                    packagedata=swt6,
                                    sig0=sig0)
    orecarray = {}
    orecarray['csub_obs.csv'] = [('w1l1', 'interbed-compaction', (0, 8, 9)),
                                 ('w1l2', 'interbed-compaction', (1, 8, 9)),
                                 ('w1l3', 'interbed-compaction', (2, 8, 9)),
                                 ('w1l4', 'interbed-compaction', (3, 8, 9)),
                                 ('w2l1', 'interbed-compaction', (0, 11, 6)),
                                 ('w2l2', 'interbed-compaction', (1, 11, 6)),
                                 ('w2l3', 'interbed-compaction', (2, 11, 6)),
                                 ('w2l4', 'interbed-compaction', (3, 11, 6)),
                                 ('w2l4q', 'csub-cell', (3, 11, 6))]
    csub_obs_package = flopy.mf6.ModflowUtlobs(gwf,
                                               fname=opth,
                                               parent_file=csub, digits=10,
                                               print_input=True,
                                               continuous=orecarray)

    # output control
    oc = flopy.mf6.ModflowGwfoc(gwf,
                                budget_filerecord='{}.cbc'.format(name),
                                head_filerecord='{}.hds'.format(name),
                                headprintrecord=[
                                    ('COLUMNS', 10, 'WIDTH', 15,
                                     'DIGITS', 6, 'GENERAL')],
                                saverecord=[('HEAD', 'ALL'),
                                            ('BUDGET', 'ALL')],
                                printrecord=[('HEAD', 'LAST'),
                                             ('BUDGET', 'ALL')])

    # build MODFLOW-2005 files
    ws = os.path.join(dir, cmppth)
    mc = flopy.modflow.Modflow(name, model_ws=ws, version=cmppth)
    dis = flopy.modflow.ModflowDis(mc, nlay=nlay, nrow=nrow, ncol=ncol,
                                   nper=nper, perlen=perlen, nstp=nstp,
                                   tsmult=tsmult, steady=steady, delr=delr,
                                   delc=delc, top=top, botm=botm)
    bas = flopy.modflow.ModflowBas(mc, ibound=ib, strt=strt, hnoflo=hnoflo)
    upw = flopy.modflow.ModflowUpw(mc, laytyp=laytyp,
                                   hk=hk, vka=hk,
                                   ss=ss, sy=sy,
                                   hdry=hdry)
    chd = flopy.modflow.ModflowChd(mc, stress_period_data=cd)
    wel = flopy.modflow.ModflowWel(mc, stress_period_data=wd)
    swt = flopy.modflow.ModflowSwt(mc, ipakcb=1001,
                                   iswtoc=1, nsystm=4,
                                   ithk=1,
                                   ivoid=ivoid[idx],
                                   istpcs=1, lnwt=[0, 1, 2, 3],
                                   cc=cc, cr=cr, thick=thick,
                                   void=void, pcsoff=ini_stress, sgs=sgs,
                                   gl0=gs0[idx], ids16=ds16, ids17=ds17)
    oc = flopy.modflow.ModflowOc(mc, stress_period_data=None,
                                 save_every=1,
                                 save_types=['save head', 'save budget',
                                             'print budget'])
    nwt = flopy.modflow.ModflowNwt(mc,
                                   headtol=hclose, fluxtol=fluxtol,
                                   maxiterout=nouter, linmeth=2,
                                   unitnumber=132,
                                   options='SPECIFIED',
                                   backflag=0, idroptol=0,
                                   hclosexmd=hclose, mxiterxmd=ninner)

    return sim, mc


def eval_comp(sim):

    print('evaluating compaction...')
    # MODFLOW 6 total compaction results
    fpth = os.path.join(sim.simpath, 'csub_obs.csv')
    try:
        tc = np.genfromtxt(fpth, names=True, delimiter=',')
    except:
        assert False, 'could not load data from "{}"'.format(fpth)

    # MODFLOW-NWT total compaction results
    cpth = cmppth
    fn = '{}.swt_total_comp.hds'.format(os.path.basename(sim.name))
    fpth = os.path.join(sim.simpath, cpth, fn)
    try:
        sobj = flopy.utils.HeadFile(fpth, text='LAYER COMPACTION')
        tc0 = sobj.get_ts((3, 11, 6))
    except:
        assert False, 'could not load data from "{}"'.format(fpth)

    # calculate maximum absolute error
    loctag = 'W2L4'
    diff = tc[loctag] - tc0[:, 1]
    diffmax = np.abs(diff).max()
    msg = 'maximum absolute total-compaction difference ({}) '.format(diffmax)

    # write summary
    fpth = os.path.join(sim.simpath,
                        '{}.comp.cmp.out'.format(os.path.basename(sim.name)))
    f = open(fpth, 'w')
    line = '{:>15s}'.format('TOTIM')
    line += ' {:>15s}'.format('CSUB')
    line += ' {:>15s}'.format('MF')
    line += ' {:>15s}'.format('DIFF')
    f.write(line + '\n')
    for i in range(diff.shape[0]):
        line = '{:15g}'.format(tc0[i, 0])
        line += ' {:15g}'.format(tc[loctag][i])
        line += ' {:15g}'.format(tc0[i, 1])
        line += ' {:15g}'.format(diff[i])
        f.write(line + '\n')
    f.close()

    if diffmax > dtol:
        sim.success = False
        msg += 'exceeds {}'.format(dtol)
        assert diffmax < dtol, msg
    else:
        sim.success = True
        print('    ' + msg)

    # compare budgets
    cbc_compare(sim)

    return


# compare cbc and lst budgets
def cbc_compare(sim):
    print('evaluating cbc and budget...')
    # open cbc file
    fpth = os.path.join(sim.simpath,
                        '{}.cbc'.format(os.path.basename(sim.name)))
    cobj = flopy.utils.CellBudgetFile(fpth, precision='double')

    # build list of cbc data to retrieve
    avail = cobj.get_unique_record_names()
    cbc_bud = []
    bud_lst = []
    for t in avail:
        if isinstance(t, bytes):
            t = t.decode()
        t = t.strip()
        if paktest in t.lower():
            cbc_bud.append(t)
            bud_lst.append('{}_IN'.format(t))
            bud_lst.append('{}_OUT'.format(t))

    # get results from listing file
    fpth = os.path.join(sim.simpath,
                        '{}.lst'.format(os.path.basename(sim.name)))
    budl = flopy.utils.Mf6ListBudget(fpth)
    names = list(bud_lst)
    d0 = budl.get_budget(names=names)[0]
    dtype = d0.dtype
    nbud = d0.shape[0]
    d = np.recarray(nbud, dtype=dtype)
    for key in bud_lst:
        d[key] = 0.

    # get data from cbc dile
    kk = cobj.get_kstpkper()
    times = cobj.get_times()
    for idx, (k, t) in enumerate(zip(kk, times)):
        for text in cbc_bud:
            qin = 0.
            qout = 0.
            v = cobj.get_data(kstpkper=k, text=text)[0]
            if isinstance(v, np.recarray):
                vt = np.zeros(size3d, dtype=np.float)
                for jdx, node in enumerate(v['node']):
                    vt[node - 1] += v['q'][jdx]
                v = vt.reshape(shape3d)
            for kk in range(v.shape[0]):
                for ii in range(v.shape[1]):
                    for jj in range(v.shape[2]):
                        vv = v[kk, ii, jj]
                        if vv < 0.:
                            qout -= vv
                        else:
                            qin += vv
            d['totim'][idx] = t
            d['time_step'][idx] = k[0]
            d['stress_period'] = k[1]
            key = '{}_IN'.format(text)
            d[key][idx] = qin
            key = '{}_OUT'.format(text)
            d[key][idx] = qout

    diff = np.zeros((nbud, len(bud_lst)), dtype=np.float)
    for idx, key in enumerate(bud_lst):
        diff[:, idx] = d0[key] - d[key]
    diffmax = np.abs(diff).max()
    msg = 'maximum absolute total-budget difference ({}) '.format(diffmax)

    # write summary
    fpth = os.path.join(sim.simpath,
                        '{}.bud.cmp.out'.format(os.path.basename(sim.name)))
    f = open(fpth, 'w')
    for i in range(diff.shape[0]):
        if i == 0:
            line = '{:>10s}'.format('TIME')
            for idx, key in enumerate(bud_lst):
                line += '{:>25s}'.format(key + '_LST')
                line += '{:>25s}'.format(key + '_CBC')
                line += '{:>25s}'.format(key + '_DIF')
            f.write(line + '\n')
        line = '{:10g}'.format(d['totim'][i])
        for idx, key in enumerate(bud_lst):
            line += '{:25g}'.format(d0[key][i])
            line += '{:25g}'.format(d[key][i])
            line += '{:25g}'.format(diff[i, idx])
        f.write(line + '\n')
    f.close()

    if diffmax > budtol:
        sim.success = False
        msg += 'exceeds {}'.format(dtol)
        assert diffmax < dtol, msg
    else:
        sim.success = True
        print('    ' + msg)

    return


# - No need to change any code below
def build_models():
    for idx, dir in enumerate(exdirs):
        sim, mc = get_model(idx, dir)
        sim.write_simulation()
        if mc is not None:
            mc.write_input()
    return


def test_mf6model():
    # determine if running on Travis
    is_travis = 'TRAVIS' in os.environ
    r_exe = None
    if not is_travis:
        if replace_exe is not None:
            r_exe = replace_exe

    # initialize testing framework
    test = testing_framework()

    # build the models
    build_models()

    # run the test models
    for idx, dir in enumerate(exdirs):
        if is_travis and not travis[idx]:
            continue
        yield test.run_mf6, Simulation(dir, exe_dict=r_exe,
                                       exfunc=eval_comp,
                                       htol=htol[idx])

    return


def main():
    # initialize testing framework
    test = testing_framework()

    # build the models
    build_models()

    # run the test models
    for dir in exdirs:
        sim = Simulation(dir, exe_dict=replace_exe,
                         exfunc=eval_comp,
                         htol=htol[idx])
        test.run_mf6(sim)

    return


# use python testmf6_csub_sub02.py --mf2005 mf2005devdbl
if __name__ == "__main__":
    # print message
    print('standalone run of {}'.format(os.path.basename(__file__)))

    # run main routine
    main()
