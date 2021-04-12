####################################
# TEMPORAL PATTERN SEPARATION TASK #
####################################



# Adapted from Daniel Bjornn's script by Nathan Muncy on 2/21/18




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
train = 1
mem1 = 0
mem2 = 0
mem3 = 0


# Behavioral/MRI
MRI = True


# win/mac
MAC = True

if MAC == False:
    from win32api import GetSystemMetrics
    scnWidth, scnHeight = (GetSystemMetrics(0), GetSystemMetrics(1))
else:
    scnWidth, scnHeight = (2560, 1440)


# test mode
TESTMODE = False

if TESTMODE == True:
    FIX_DUR = 0.1
    STIM_DUR = 0.1
    STAT_DUR = 0.1
    RESP_DUR = 0.1
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
STIM_PAR = '../../Stimuli/'
MSTIM_DIR = STIM_PAR + 'StimSet/'
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
# here we read in the .txt file that specifies all the trials - I created these in R (Generate_Stim_Lists.R)
# Create TRIALS lists for each portion of experiment

## Train
TrainTrial = []
TrainTrial_FILE = open(SUBJ_DIR + SUBJ_INFO['Subject Number'] + '_Train_stimuli.txt', 'r')
TrainTrial_FILE.readline()  # skip the first line, i.e., the header
for l in TrainTrial_FILE:
    TrainTrial.append(l.split())  # read all lines into the "trials" list

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




### Visuals

# Response scrren
COLOR = (-1, -1, -1)
HEIGHT = 40

FIX = visual.TextStim(WIN, text='+', color=COLOR, height=HEIGHT)
E1 = visual.TextStim(WIN, text='B - Smooth\n\nN - Rough\n\nM - Sharp', color=COLOR, height=HEIGHT)
E2 = visual.TextStim(WIN, text='B - Metal\n\nN - Plastic\n\nM - Other', color=COLOR, height=HEIGHT)
E3 = visual.TextStim(WIN, text='B - Heavy\n\nN - Medium\n\nM - Light', color=COLOR, height=HEIGHT)
TIME_ANS = visual.TextStim(WIN, text='B - Longer\n\nN - Same Time\n\nM - Shorter', color=COLOR, height=HEIGHT)

# wait screens
if MRI == True:
    MSG = visual.TextStim(WIN, text="Please wait for initial scans", color=COLOR, units="pix", height=HEIGHT)
    START_BLOCK = visual.TextStim(WIN, text="Starting next block.\n\nWaiting for scanner ...", color=COLOR, units="pix", height=HEIGHT)
    FINISH = visual.TextStim(WIN, text="You have finished!\n\nThere are just a few scans left.", color = COLOR, units = "pix", height = HEIGHT)
else:
    MSG = visual.TextStim(WIN, text="Please wait for instructions from the researcher.", color=COLOR, units="pix", height=HEIGHT)
    START_BLOCK = visual.TextStim(WIN, text="Starting next block.", color=COLOR, units="pix", height=HEIGHT)
    FINISH = visual.TextStim(WIN, text="You have finished!\n\n", color = COLOR, units = "pix", height = HEIGHT)

POST_MOVIE = visual.TextStim(WIN, text="Any questions?", color = COLOR, units = "pix", height = HEIGHT)


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
    IMG = visual.ImageStim(WIN, image = STIM_PAR + 'Static_Color.png')
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
        keyP = event.getKeys(['b', 'n', 'm', 'B', 'N', 'M', '1', '2', '3', '4', 'q', 'Q', 'escape'])
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







## Video function
def playMovie(filename):
    videopath = (STIM_PAR + 'Instructions/' + filename)

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




## Do Train, with video
##------------------------
if train == 1:

    # video, wait screen
    LOG.write("Starting First Movie, %f\n" % core.getTime())
    if TESTMODE == True:
        playMovie("debug_video.mp4")
    else:
        if MRI == True:
            playMovie("SOUND_Temporal_Instruction_MRI.mp4")
        else:
            playMovie("SOUND_Temporal_Instruction_Behavioral.mp4")
    WIN.flip() # clear video

    # post screen
    POST_MOVIE.draw()
    WIN.flip()
    event.waitKeys(keyList = ["p"])

    POST_Block = visual.TextStim(WIN, text="Let's do a quick practice.", color = COLOR, units = "pix", height = HEIGHT)
    POST_Block.draw()
    WIN.flip()
    event.waitKeys(keyList = ["p"])

    # first fixation
    triggerTime = core.getTime()
    FIX.draw()
    WIN.flip()
    while core.getTime() - triggerTime <= OFFSET_TIME:
        pass


    # do train
    for i in range(len(TrainTrial)):

        # retrieve paramters
        Trial, Block, File, Duration, Encoding, StimType, Lag, Repeat = TrainTrial[i]

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

        TRIAL_DUR = MemoryTrialFunction(TrainTrial[i], TRIAL_DUR)


    # pause after block
    POST_Block = visual.TextStim(WIN, text="Do you have any questions?", color = COLOR, units = "pix", height = HEIGHT)
    POST_Block.draw()
    WIN.flip()
    event.waitKeys(keyList = ["p"])




## Do Memory 1
##------------------------
if mem1 == 1:

    # wait screen
    START_BLOCK.draw()
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
    POST_Block = visual.TextStim(WIN, text="Well done - you have finished block 1/3. \n\nPlease wait to continue.", color = COLOR, units = "pix", height = HEIGHT)
    POST_Block.draw()
    WIN.flip()
    event.waitKeys(keyList = ["p"])



## Do Memory 2
##------------------------
if mem2 == 1:

    # wait screen
    START_BLOCK.draw()
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
    POST_Block = visual.TextStim(WIN, text="Well done - you have finished block 2/3. \n\nPlease wait to continue.", color = COLOR, units = "pix", height = HEIGHT)
    POST_Block.draw()
    WIN.flip()
    event.waitKeys(keyList = ["p"])





## Do Memory 3
##------------------------
if mem3 == 1:

    # wait screen
    START_BLOCK.draw()
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
    POST_Block = visual.TextStim(WIN, text="Well done - you have finished block 3/8. \n\nPlease wait to continue.", color = COLOR, units = "pix", height = HEIGHT)
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

# quit the program
WIN.close()
core.quit()








