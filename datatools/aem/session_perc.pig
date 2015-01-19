/*
Extract user perception measures within sessions.

Usage: pig -param input=inpath -param output=outpath\
    session_perc.pig

Input: Session features extracted from HTTP logs.
Output: Selected user perception fields within sessions.
*/

-- for debugging
-- %declare input '/home/chenxm/Downloads/acts/sFeatures.out';
-- %declare output '/home/chenxm/Downloads/acts/session_perc.out';

import 'commons.pig';

session = M_SESSION_FEATURE_LOADER('$input');

-- Filter out sessions with non-interest value range
session = filter session by
    volume >= 10 and --<< Change
    ADur <= 600 and --<< Change
    MWTime <= 60; --<< Change

-- Filter out users with less sessions
ugrp = group session by UID;
ugrp = filter ugrp by COUNT(session) >= 1; --<< Change
session_filtered = foreach ugrp generate
    flatten(group),
    flatten(session);

-- Generate session engagement and perceptual metrics
session_ord = order session_filtered by UID, serviceCat, STime;
DUR = foreach session_ord generate
    UID, serviceCat, STime, volume,
    size, DSL, IR, VF,
    ADur, MWTime, PABw, building;

-- Save to disk
store DUR into '$output';