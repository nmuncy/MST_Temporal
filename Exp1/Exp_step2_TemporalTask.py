####################################
# TEMPORAL PATTERN SEPARATION TASK #
####################################



# Adapted from Daniel Bjornn's script by Nathan Muncy on 2/21/18


# Updated on 6/5/18 to:
# 	decrease the number of Target/Lure durations from 4 (2, 1.5, 1, 0.5 seconds) to 2 (1.5, 1 seconds).
#
#	This means that two types of lures now exist a) 1s -> 1.5s Lure, and b) 1.5s -> 1s Lure.
# 	I'll have to check to see if the differing study/test times affect the performance,
#	and pilot until I find the right Lure change so that it is detectable but doesn't
#	impact performance.



### import libraries
from __future__ import division, absolute_import
import time, os
from psychopy import visual, core, event, monitors, sound
from psychopy.constants import (NOT_STARTED, STARTED, PLAYING, PAUSED, STOPPED, FINISHED, PRESSED, RELEASED, FOREVER)






####       RAs !!!!!        #####
##
##
##     Please check the toggles!
##
##
## ------------------------  #####





### Toggles

# blocks, for testing
mem1 = 1
mem2 = 1
mem3 = 1
mem4 = 1
mem5 = 1
mem6 = 1
pos1 = 1
pos2 = 1


# Behavioral/MRI
MRI = False


# win/mac
MAC = False

if MAC == False:
    from win32api import GetSystemMetrics
    scnWidth, scnHeight = (GetSystemMetrics(0), GetSystemMetrics(1))
else:
    scnWidth, scnHeight = (2048, 1152)


# test mode
TESTMODE = False

if TESTMODE == True:
    FIX_DUR = 0.1
    STIM_DUR = 0.1
    PSTIM_DUR = 0.1
    STAT_DUR = 0.1
    RESP_DUR = 0.1
    ISI_DUR = 0.1
    CUE_DUR = 0.1
    OFFSET_TIME = 0.1
    INSTRUCTION_DUR = 0.1
    PRE_VID_DUR = 0.1
    TRIAL_DUR = 0.1





### Thank You  RAs ###
##------------------##







##### Real code starts here



### Get subject info
# use the Psychopy GUI module to collect subject information
useGUI = True
SUBJ_INFO = {'Subject Number': '', 'Computer Number': '', 'Age': '', 'Sex': ''}
if useGUI:
    from psychopy import gui
    dlg = gui.DlgFromDict(dictionary=SUBJ_INFO, title="Subject Information", order=['Computer Number', 'Subject Number', 'Age', 'Sex'])
    if dlg.OK is False:
        core.quit()  # user pressed cancel
else:
    SUBJ_INFO['Subject Number'] = raw_input('Subject #: ')


## Set variables
WORK_DIR = '../'
SUBJ_DIR = WORK_DIR + SUBJ_INFO['Subject Number'] + '/'
MSTIM_DIR = WORK_DIR + 'Stimuli/StimSet/'
PSTIM_DIR = WORK_DIR + 'Stimuli/Posner_stimuli/'
LOG = open(SUBJ_DIR + SUBJ_INFO['Subject Number'] + '_timing_log.csv', 'w')

# write log file
LOG.write("Subject Number, %s\n" % SUBJ_INFO['Subject Number'])
LOG.write("Computer Number, %s\n" % SUBJ_INFO['Computer Number'])
LOG.write("Subject Age, %s\n" % SUBJ_INFO['Age'])
LOG.write("Subject Sex, %s\n" % SUBJ_INFO['Sex'])

# Open a csv data file EARLY
DATA_FILE_NAME = SUBJ_INFO['Subject Number'] + '.csv'  # edf




### Initialize custom graphics for camera setup & drift correction
# set the resolution to the monitor, create visual, hide mouse
MON = monitors.Monitor('monitor')
MON.setSizePix((scnWidth, scnHeight))
WIN = visual.Window((scnWidth, scnHeight), fullscr=True, monitor=MON, color=[0, 0, 0], units='pix')
WIN.setMouseVisible(False)




### Specify all possible experimental cells
# here we read in the .txt file that specifies all the trials
# Create TRIALS lists for each portion of experiment

## Memory
MemTrial1 = []
MemTrial1_FILE = open(SUBJ_DIR + SUBJ_INFO['Subject Number'] + '_B1_stimuli.txt', 'r')
MemTrial1_FILE.readline()  # skip the first line, i.e., the header
for l in MemTrial1_FILE:
    MemTrial1.append(l.split())  # read all lines into the "trials" list

MemTrial2 = []
MemTrial2_FILE = open(SUBJ_DIR + SUBJ_INFO['Subject Number'] + '_B2_stimuli.txt', 'r')
MemTrial2_FILE.readline()
for l in MemTrial2_FILE:
    MemTrial2.append(l.split())

MemTrial3 = []
MemTrial3_FILE = open(SUBJ_DIR + SUBJ_INFO['Subject Number'] + '_B3_stimuli.txt', 'r')
MemTrial3_FILE.readline()
for l in MemTrial3_FILE:
    MemTrial3.append(l.split())

MemTrial4 = []
MemTrial4_FILE = open(SUBJ_DIR + SUBJ_INFO['Subject Number'] + '_B4_stimuli.txt', 'r')
MemTrial4_FILE.readline()
for l in MemTrial4_FILE:
    MemTrial4.append(l.split())

MemTrial5 = []
MemTrial5_FILE = open(SUBJ_DIR + SUBJ_INFO['Subject Number'] + '_B5_stimuli.txt', 'r')
MemTrial5_FILE.readline()
for l in MemTrial5_FILE:
    MemTrial5.append(l.split())

MemTrial6 = []
MemTrial6_FILE = open(SUBJ_DIR + SUBJ_INFO['Subject Number'] + '_B6_stimuli.txt', 'r')
MemTrial6_FILE.readline()
for l in MemTrial6_FILE:
    MemTrial6.append(l.split())


## Posner
PosTrial1 = []
PosTrial1_FILE = open(SUBJ_DIR + SUBJ_INFO['Subject Number'] + '_P1_stimuli.txt', 'r')
PosTrial1_FILE.readline()
for l in PosTrial1_FILE:
    PosTrial1.append(l.split())

PosTrial2 = []
PosTrial2_FILE = open(SUBJ_DIR + SUBJ_INFO['Subject Number'] + '_P2_stimuli.txt', 'r')
PosTrial2_FILE.readline()
for l in PosTrial2_FILE:
    PosTrial2.append(l.split())




### Visuals
# words
COLOR = (-1, -1, -1)
HEIGHT = 40

FIX = visual.TextStim(WIN, text='+', color=COLOR, height=HEIGHT)
E1 = visual.TextStim(WIN, text='B - Smooth\nN - Rough\nM - Sharp', color=COLOR, height=HEIGHT)
E2 = visual.TextStim(WIN, text='B - Metal\nN - Plastic\nM - Other', color=COLOR, height=HEIGHT)
E3 = visual.TextStim(WIN, text='B - Heavy\nN - Medium\nM - Light', color=COLOR, height=HEIGHT)
TIME_ANS = visual.TextStim(WIN, text='B - Longer\nN - Same Time\nM - Shorter', color=COLOR, height=HEIGHT)

# posner pics
PFIX = visual.ImageStim(WIN, image = PSTIM_DIR + 'Posner_fix.jpeg')
PL_CUE = visual.ImageStim(WIN, image = PSTIM_DIR + 'Posner_left_cue.jpeg')
PR_CUE = visual.ImageStim(WIN, image = PSTIM_DIR + 'Posner_right_cue.jpeg')
PL_TAR = visual.ImageStim(WIN, image = PSTIM_DIR + 'Posner_left_target.jpeg')
PR_TAR = visual.ImageStim(WIN, image = PSTIM_DIR + 'Posner_right_target.jpeg')

# wait screens
if MRI == True:
    MSG = visual.TextStim(WIN, text="Please wait for initial scans", color=COLOR, units="pix", height=HEIGHT)
    POST_MOVIE = visual.TextStim(WIN, text="Any questions?\n\nWaiting for scanner.", color = COLOR, units = "pix", height = HEIGHT)
    FINISH = visual.TextStim(WIN, text="You have finished!\n\nThere are just a few scans left.", color = COLOR, units = "pix", height = HEIGHT)
else:
    MSG = visual.TextStim(WIN, text="Please wait for instructions from the researcher.", color=COLOR, units="pix", height=HEIGHT)
    POST_MOVIE = visual.TextStim(WIN, text="Any questions?\n\n", color = COLOR, units = "pix", height = HEIGHT)
    FINISH = visual.TextStim(WIN, text="You have finished!\n\n", color = COLOR, units = "pix", height = HEIGHT)



### Timing
if TESTMODE == False:

    # offset, fixation
    OFFSET_TIME = 3.6
    FIX_DUR = 0.5

    # memory
    RESP_DUR = 2.5
    STAT_DUR = 0.3
    T1_LONG = 1.5
    T2_SHORT = 1
    L1_LONG = 1.5
    L2_SHORT = 1

    # posner
    CUE_DUR = 0.2
    PSTIM_DUR = 0.5




### Memory Trial function
TOTAL_MTRIAL_DURATION = 0
def MemoryTrialFunction(pars, PREVIOUS_TRIAL_DURATION = TOTAL_MTRIAL_DURATION):

    """
        This function runs a single trial (or stimulus) based on the line in the
        file.
        :param pars: corresponds to a row in the trial list
        :return trialDuration: corresponds to the length of the trial to keep
        everything on track
        """

    # Start log, retrieve paramters from the trial list
    LOG.write("Trial Start, %f\n" % core.getTime())
    Trial, Block, File, Duration, Encoding, StimType, Lag, Repeat = pars


    ## Fixation
    # get the trial start time, draw
    TRIAL_START_TIME = core.getTime()
    FIX.draw()
    WIN.flip()

    # adjust presentation time for previous slow loading
    ADJ_FIX_DUR = (FIX_DUR - (PREVIOUS_TRIAL_DURATION - TRIAL_DUR))
    while core.getTime() - TRIAL_START_TIME <= ADJ_FIX_DUR:
        pass


    ## Stimulus
    IMG = visual.ImageStim(WIN, image = MSTIM_DIR + File)
    IMG.draw()
    LOG.write("Stimulus Onset, %f\n" % core.getTime())
    WIN.flip()

    # stop showing stimulus @ appropriate time
    timeOut = False
    while not timeOut:
        if core.getTime() - TRIAL_START_TIME >= (ADJ_FIX_DUR + STIM_DUR):
            timeOut = True


    ## Static
    IMG = visual.ImageStim(WIN, image = WORK_DIR + 'Stimuli/Static_Color.png')
    IMG.draw()
    WIN.flip()
    LOG.write("Static Onset, %f\n" % core.getTime())

    # time out
    timeOut = False
    while not timeOut:
        if core.getTime() - TRIAL_START_TIME >= (ADJ_FIX_DUR + STIM_DUR + STAT_DUR):
            timeOut = True

    ## Extract other variables
    stimdur = Duration
    stimtyp = StimType
    stimlag = Lag
    stimrep = Repeat

    ## Response
    # determine
    if Repeat == "No":
        if Encoding == "E1":
            options = E1
        elif Encoding == "E2":
            options = E2
        elif Encoding == "E3":
            options = E3
    elif Repeat == "Yes":
        options = TIME_ANS

    # write out stimulus info
    LOG.write("stim_dur, %s\n" % stimdur)
    LOG.write("stim_typ, %s\n" % stimtyp)
    LOG.write("stim_lag, %s\n" % stimlag)
    LOG.write("stim_rep, %s\n" % stimrep)

    options.draw()
    WIN.flip()
    LOG.write("Resp Onset, %f\n" % core.getTime())
    optionKey = '-1'
    event.clearEvents()  # clear cached (keyboard/mouse etc.) events

    # show response
    timeOut = False
    while not timeOut:

        # catch responses
        keyP = event.getKeys(['b', 'n', 'm', 'q', 'B', 'N', 'M', 'Q', 'escape'])
        if len(keyP) > 0:

            optionKey = keyP[0]
            LOG.write("Resp Timing, %f\n" % core.getTime())
            LOG.write("Response, %s\n" % optionKey)

            # debugging kill
            if optionKey in ['q', 'Q', 'escape']:
                WIN.close()
                core.quit()

        # Time out
        if core.getTime() - TRIAL_START_TIME >= (ADJ_FIX_DUR + STIM_DUR + STAT_DUR + RESP_DUR):
            timeOut = True


    ## Log close of trial
    LOG.write("Trial End, %f\n" % core.getTime())
    TOTAL_MTRIAL_DURATION = (core.getTime()-TRIAL_START_TIME)
    return TOTAL_MTRIAL_DURATION




### Posner Trial function
TOTAL_PTRIAL_DURATION = 0
def PosnerTrialFunction(pars, PREVIOUS_TRIAL_DURATION = TOTAL_PTRIAL_DURATION):

    # start log
    LOG.write("Trial Start, %f\n" % core.getTime())
    Trial, Block, Duration, StimType = pars


    ## Presner Fixation
    TRIAL_START_TIME = core.getTime()
    PFIX.draw()
    WIN.flip()

    ADJ_FIX_DUR = (FIX_DUR - (PREVIOUS_TRIAL_DURATION - TRIAL_DUR))
    while core.getTime() - TRIAL_START_TIME <= ADJ_FIX_DUR:
        pass


    ## Cue
    if (StimType == "ConLeft") or (StimType == "IConLeft"):
        HOLD_cue = PL_CUE
    elif (StimType == "ConRight") or (StimType == "IConRight"):
        HOLD_cue = PR_CUE

    HOLD_cue.draw()
    WIN.flip()
    LOG.write("Cue Onset, %f\n" % core.getTime())

    timeOut = False
    while not timeOut:
        if core.getTime() - TRIAL_START_TIME >= (ADJ_FIX_DUR + CUE_DUR):
            timeOut = True


    ## ISI
    PFIX.draw()
    WIN.flip()
    LOG.write("ISI Onset, %f\n" % core.getTime())

    timeOut = False
    while not timeOut:
        if core.getTime() - TRIAL_START_TIME >= (ADJ_FIX_DUR + CUE_DUR + ISI_DUR):
            timeOut = True


    ## Stimulus
    if (StimType == "ConLeft") or (StimType == "IConRight"):
        HOLD_tar = PL_TAR
    elif (StimType == "ConRight") or (StimType == "IConLeft"):
        HOLD_tar = PR_TAR

    keyPressed = '-1'
    event.clearEvents()
    HOLD_tar.draw()
    WIN.flip()
    LOG.write("Stimulus Onset, %f\n" % core.getTime())

    # show response
    timeOut = False
    while not timeOut:

        # catch responses
        keyP = event.getKeys(['b', 'm', 'q', 'B', 'M', 'Q', 'escape'])
        if len(keyP) > 0:

            optionKey = keyP[0]
            LOG.write("Resp Timing, %f\n" % core.getTime())
            LOG.write("Response, %s\n" % optionKey)

            # debugging kill
            if optionKey in ['q', 'Q', 'escape']:
                WIN.close()
                core.quit()

        # Time out
        if core.getTime() - TRIAL_START_TIME >= (ADJ_FIX_DUR + CUE_DUR + ISI_DUR + PSTIM_DUR):
            timeOut = True


    LOG.write("Trial End, %f\n" % core.getTime())
    TOTAL_PTRIAL_DURATION = core.getTime()-TRIAL_START_TIME
    return TOTAL_PTRIAL_DURATION




## Video function
def playMovie(filename):
    videopath = (WORK_DIR + 'Stimuli/Instructions/' + filename)

    # create movie stim
    MOV = visual.MovieStim2(WIN, videopath, size = 1920, flipVert = False, flipHoriz = False, loop = False)

    # start the movie stim by preparing it to play
    shouldflip = MOV.play()
    while MOV.status != visual.FINISHED:
        # Only flip when a new frame should be displayed. Can significantly reduce
        # CPU usave. This only makes sense if the movie is the only /dynamic/ stim
        # displayed.
        if shouldflip:
            # Movie has already been drawn, so just draw text stim and flip
            WIN.flip()
        else:
            # Give the OS a break if a flip is not needed
            time.sleep(0.001)
        # Drawn movie stim again. Updating of movie stim frames as necessary
        # is handled internally.
        shouldflip = MOV.draw()

        # Check for action keys....
        for key in event.getKeys():
            if key in ['escape', 'q']:
                WIN.close()
                core.quit()
            elif key in ['s', ]:
                if MOV.status in [visual.PLAYING, visual.PAUSED]:
                    # To stop the movie being played.....
                    MOV.stop()
                    # Clear screen of last displayed frame.
                    WIN.flip()
                    # When movie stops, clear screen of last displayed frame
                    WIN.flip()
                else:
                    # To replay a movie that was stopped.....
                    MOV.loadMovie(videopath)
                    shouldflip = MOV.play()
            elif key in ['p', ]:
                # To pause the movie while it is playing....
                if MOV.status == visual.PLAYING:
                    MOV.pause()
                elif MOV.status == visual.PAUSED:
                    # To /unpause/ the movie if pause has been called....
                    MOV.play()
                    WIN.flip()
            elif key == 'period':
                # To skip ahead 1 second in movie.
                ntime = min(MOV.getCurrentFrameTime() + 1.0, MOV.duration)
                MOV.seek(ntime)
            elif key == 'comma':
                # To skip back 1 second in movie ....
                ntime = max(MOV.getCurrentFrameTime() - 1.0, 0.0)
                MOV.seek(ntime)
            elif key == 'minus':
                # To decrease movie sound a bit ....
                CV = max(MOV.getVolume() - 5, 0)
                MOV.setVolume(CV)
            elif key == 'equal':
                # To increase movie sound a bit ....
                CV = MOV.getVolume()
                CV = min(MOV.getVolume() + 5, 100)
                MOV.setVolume(CV)




###################
### Do protocol ###
###################

## show starting instructions page
pre_vid_onset = core.getTime()
MSG.draw()
WIN.flip()

if TESTMODE == True:
    if core.getTime() - pre_vid_onset >= PRE_VID_DUR:
       timeOut = True
else:
    event.waitKeys(keyList=["p"])




## Do Memory 1, with video
##------------------------
if mem1 == 1:

    # video, wait screen
    LOG.write("Starting First Movie, %f\n" % core.getTime())
    if TESTMODE == True:
        playMovie("debug_video.mp4")
    else:
        playMovie("Temporal_1.mp4")
    WIN.flip() # clear video

    # post screen
    POST_MOVIE.draw()
    WIN.flip()
    event.waitKeys(keyList = ["5"])

    # first fixation
    triggerTime = core.getTime()
    FIX.draw()
    WIN.flip()
    while core.getTime() - triggerTime <= OFFSET_TIME:
        pass


    # do B1
    for i in range(len(MemTrial1)):

        # retrieve paramters
        Trial, Block, File, Duration, Encoding, StimType, Lag, Repeat = MemTrial1[i]

        # determine timing, submit trial functions
        if TESTMODE == False:
            if Repeat == "No":
                if Duration == "T1":
                    STIM_DUR = float(T1_LONG)
                else:
                    STIM_DUR = float(T2_SHORT)
            else:
                if Duration == "T1":
                    if StimType == "Targ":
                        STIM_DUR = float(T1_LONG)
                    else:
                        STIM_DUR = float(L2_SHORT)
                else:
                    if StimType == "Targ":
                        STIM_DUR = float(T2_SHORT)
                    else:
                        STIM_DUR = float(L1_LONG)

            TRIAL_DUR = (FIX_DUR + STIM_DUR + STAT_DUR + RESP_DUR)

        TRIAL_DUR = MemoryTrialFunction(MemTrial1[i], TRIAL_DUR)


    # pause after block
    POST_Block = visual.TextStim(WIN, text="Well done - you have finished block 1/8. \n\nPlease wait to continue.", color = COLOR, units = "pix", height = HEIGHT)
    POST_Block.draw()
    WIN.flip()
    event.waitKeys(keyList = ["p"])




## Do Memory 2
##------------------------
if mem2 == 1:

    # wait screen
    POST_MOVIE.draw()
    WIN.flip()
    event.waitKeys(keyList = ["5"])

    # first fixation
    triggerTime = core.getTime()
    FIX.draw()
    WIN.flip()
    while core.getTime() - triggerTime <= OFFSET_TIME:
        pass


    # do B2
    for i in range(len(MemTrial2)):

        # retrieve paramters
        Trial, Block, File, Duration, Encoding, StimType, Lag, Repeat = MemTrial2[i]

        # determine timing, submit trial functions
        if TESTMODE == False:
            if Repeat == "No":
                if Duration == "T1":
                    STIM_DUR = float(T1_LONG)
                else:
                    STIM_DUR = float(T2_SHORT)
            else:
                if Duration == "T1":
                    if StimType == "Targ":
                        STIM_DUR = float(T1_LONG)
                    else:
                        STIM_DUR = float(L2_SHORT)
                else:
                    if StimType == "Targ":
                        STIM_DUR = float(T2_SHORT)
                    else:
                        STIM_DUR = float(L1_LONG)

            TRIAL_DUR = (FIX_DUR + STIM_DUR + STAT_DUR + RESP_DUR)

        TRIAL_DUR = MemoryTrialFunction(MemTrial2[i], TRIAL_DUR)


    # pause after block
    POST_Block = visual.TextStim(WIN, text="Well done - you have finished block 2/8. \n\nPlease wait to continue.", color = COLOR, units = "pix", height = HEIGHT)
    POST_Block.draw()
    WIN.flip()
    event.waitKeys(keyList = ["p"])




## Do Posner 1, with video
##-----------------------
if pos1 == 1:

    # video, wait screen
    LOG.write("Starting Second Movie, %f\n" % core.getTime())
    if TESTMODE == True:
        playMovie("debug_video.mp4")
    else:
        playMovie("Posner_1.mp4")
    WIN.flip() # clear video


    POST_MOVIE.draw()
    WIN.flip()
    event.waitKeys(keyList = ["5"])


    # first fixation
    triggerTime = core.getTime()
    PFIX.draw()
    WIN.flip()

    while core.getTime() - triggerTime <= OFFSET_TIME:
        pass


    # do P1
    for i in range(len(PosTrial1)):

        # retrieve paramters
        Trial, Block, Duration, StimType = PosTrial1[i]

        # determine timing, submit trial functions
        if TESTMODE == False:
            ISI_DUR = float(Duration)
            TRIAL_DUR = (FIX_DUR + CUE_DUR + ISI_DUR + PSTIM_DUR)

        TRIAL_DUR = PosnerTrialFunction(PosTrial1[i], TRIAL_DUR)


    # pause after block
    POST_Block = visual.TextStim(WIN, text="Well done - you have finished block 3/8. \n\nPlease wait to continue.", color = COLOR, units = "pix", height = HEIGHT)
    POST_Block.draw()
    WIN.flip()
    event.waitKeys(keyList = ["p"])




## Do Memory 3
##------------------------
if mem3 == 1:

    # wait screen
    POST_MOVIE.draw()
    WIN.flip()
    event.waitKeys(keyList = ["5"])


    # first fixation
    triggerTime = core.getTime()
    FIX.draw()
    WIN.flip()

    while core.getTime() - triggerTime <= OFFSET_TIME:
        pass


    # do B3
    for i in range(len(MemTrial3)):

        # retrieve paramters
        Trial, Block, File, Duration, Encoding, StimType, Lag, Repeat = MemTrial3[i]

        # determine timing, submit trial functions
        if TESTMODE == False:
            if Repeat == "No":
                if Duration == "T1":
                    STIM_DUR = float(T1_LONG)
                else:
                    STIM_DUR = float(T2_SHORT)
            else:
                if Duration == "T1":
                    if StimType == "Targ":
                        STIM_DUR = float(T1_LONG)
                    else:
                        STIM_DUR = float(L2_SHORT)
                else:
                    if StimType == "Targ":
                        STIM_DUR = float(T2_SHORT)
                    else:
                        STIM_DUR = float(L1_LONG)

            TRIAL_DUR = (FIX_DUR + STIM_DUR + STAT_DUR + RESP_DUR)

        TRIAL_DUR = MemoryTrialFunction(MemTrial3[i], TRIAL_DUR)


    # pause after block
    POST_Block = visual.TextStim(WIN, text="Well done - you have finished block 4/8. \n\nPlease wait to continue.", color = COLOR, units = "pix", height = HEIGHT)
    POST_Block.draw()
    WIN.flip()
    event.waitKeys(keyList = ["p"])




## Do Memory 4
##------------------------
if mem4 == 1:

    # wait screen
    POST_MOVIE.draw()
    WIN.flip()
    event.waitKeys(keyList = ["5"])


    # first fixation
    triggerTime = core.getTime()
    FIX.draw()
    WIN.flip()

    while core.getTime() - triggerTime <= OFFSET_TIME:
        pass


    # do B4
    for i in range(len(MemTrial4)):

        # retrieve paramters
        Trial, Block, File, Duration, Encoding, StimType, Lag, Repeat = MemTrial4[i]

        # determine timing, submit trial functions
        if TESTMODE == False:
            if Repeat == "No":
                if Duration == "T1":
                    STIM_DUR = float(T1_LONG)
                else:
                    STIM_DUR = float(T2_SHORT)
            else:
                if Duration == "T1":
                    if StimType == "Targ":
                        STIM_DUR = float(T1_LONG)
                    else:
                        STIM_DUR = float(L2_SHORT)
                else:
                    if StimType == "Targ":
                        STIM_DUR = float(T2_SHORT)
                    else:
                        STIM_DUR = float(L1_LONG)

            TRIAL_DUR = (FIX_DUR + STIM_DUR + STAT_DUR + RESP_DUR)

        TRIAL_DUR = MemoryTrialFunction(MemTrial4[i], TRIAL_DUR)


    # pause after block
    POST_Block = visual.TextStim(WIN, text="Well done - you have finished block 5/8. \n\nPlease wait to continue.", color = COLOR, units = "pix", height = HEIGHT)
    POST_Block.draw()
    WIN.flip()
    event.waitKeys(keyList = ["p"])




## Do Posner 2
##-----------------------
if pos2 == 1:

    # wait screen
    POST_MOVIE.draw()
    WIN.flip()
    event.waitKeys(keyList = ["5"])


    # first fixation
    triggerTime = core.getTime()
    PFIX.draw()
    WIN.flip()

    while core.getTime() - triggerTime <= OFFSET_TIME:
        pass


    # do P2
    for i in range(len(PosTrial2)):

        # retrieve paramters
        Trial, Block, Duration, StimType = PosTrial2[i]

        # determine timing, submit trial functions
        if TESTMODE == False:
            ISI_DUR = float(Duration)
            TRIAL_DUR = (FIX_DUR + CUE_DUR + ISI_DUR + PSTIM_DUR)

        TRIAL_DUR = PosnerTrialFunction(PosTrial2[i], TRIAL_DUR)


    # pause after block
    POST_Block = visual.TextStim(WIN, text="Well done - you have finished block 6/8. \n\nPlease wait to continue.", color = COLOR, units = "pix", height = HEIGHT)
    POST_Block.draw()
    WIN.flip()
    event.waitKeys(keyList = ["p"])




## Do Memory 5
##------------------------
if mem5 == 1:

    # wait screen
    POST_MOVIE.draw()
    WIN.flip()
    event.waitKeys(keyList = ["5"])


    # first fixation
    triggerTime = core.getTime()
    FIX.draw()
    WIN.flip()

    while core.getTime() - triggerTime <= OFFSET_TIME:
        pass


    # do B5
    for i in range(len(MemTrial5)):

        # retrieve paramters
        Trial, Block, File, Duration, Encoding, StimType, Lag, Repeat = MemTrial5[i]

        # determine timing, submit trial functions
        if TESTMODE == False:
            if Repeat == "No":
                if Duration == "T1":
                    STIM_DUR = float(T1_LONG)
                else:
                    STIM_DUR = float(T2_SHORT)
            else:
                if Duration == "T1":
                    if StimType == "Targ":
                        STIM_DUR = float(T1_LONG)
                    else:
                        STIM_DUR = float(L2_SHORT)
                else:
                    if StimType == "Targ":
                        STIM_DUR = float(T2_SHORT)
                    else:
                        STIM_DUR = float(L1_LONG)

            TRIAL_DUR = (FIX_DUR + STIM_DUR + STAT_DUR + RESP_DUR)

        TRIAL_DUR = MemoryTrialFunction(MemTrial5[i], TRIAL_DUR)


    # pause after block
    POST_Block = visual.TextStim(WIN, text="Well done - you have finished block 7/8. \n\nPlease wait to continue.", color = COLOR, units = "pix", height = HEIGHT)
    POST_Block.draw()
    WIN.flip()
    event.waitKeys(keyList = ["p"])




## Do Memory 6
##------------------------
if mem6 == 1:

    # wait screen
    POST_MOVIE.draw()
    WIN.flip()
    event.waitKeys(keyList = ["5"])


    # first fixation
    triggerTime = core.getTime()
    FIX.draw()
    WIN.flip()

    while core.getTime() - triggerTime <= OFFSET_TIME:
        pass


    # do B6
    for i in range(len(MemTrial6)):

        # retrieve paramters
        Trial, Block, File, Duration, Encoding, StimType, Lag, Repeat = MemTrial6[i]

        # determine timing, submit trial functions
        if TESTMODE == False:
            if Repeat == "No":
                if Duration == "T1":
                    STIM_DUR = float(T1_LONG)
                else:
                    STIM_DUR = float(T2_SHORT)
            else:
                if Duration == "T1":
                    if StimType == "Targ":
                        STIM_DUR = float(T1_LONG)
                    else:
                        STIM_DUR = float(L2_SHORT)
                else:
                    if StimType == "Targ":
                        STIM_DUR = float(T2_SHORT)
                    else:
                        STIM_DUR = float(L1_LONG)

            TRIAL_DUR = (FIX_DUR + STIM_DUR + STAT_DUR + RESP_DUR)

        TRIAL_DUR = MemoryTrialFunction(MemTrial6[i], TRIAL_DUR)


    # pause after block
    POST_Block = visual.TextStim(WIN, text="Well done - you have finished block 8/8. \n\nPlease wait to continue.", color = COLOR, units = "pix", height = HEIGHT)
    POST_Block.draw()
    WIN.flip()
    event.waitKeys(keyList = ["p"])




### Finish screen
FINISH.draw()
WIN.flip()
event.waitKeys(keyList = ["escape"])




### Shut down, save
# mark end of task
LOG.write("Data transfering, %f\n" % core.getTime())

# Close the log file
LOG.close()

## transfer the CSV file to the R drive (needs to be changed for temporal task)
#os.system('copy ' + os.getcwd() + '\\subjects\\' + SUBJ_INFO['Subject Number'] + '\\' +
#    SUBJ_INFO['Subject Number'] + '_timing_log.csv' + ' R:\\KirwanLab\\auditory\\behavioral\\program\\subjects\\' +
#    SUBJ_INFO['Subject Number'] + '\\' + SUBJ_INFO['Subject Number'] + '_timing_log.csv')

# quit the program
WIN.close()
core.quit()








