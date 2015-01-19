/*
This is a wrapper around commons.pig to extract activity features I
am interested in.

Input: HTTP logs labeled with activty and session IDs,
    output by `DetectActivity.pig`
Output: Features of user activities and sessions.

@Author: chenxm - chenxm35@gmail.com
*/

-- for debugging
-- %declare input '/home/chenxm/Downloads/acts/http.out'

import 'commons.pig';

-- Loading activity-labeled HTTP records
http = M_LOAD_HTTP('$input');

-- Generate activity features
aFeatures = M_ACTIVITY_FEATURES(http);

store aFeatures into 'features_act.out'

-- Activity features about perception
cats = foreach aFeatures generate
    subCat1, volume, size,
    ADur, MWTime, MEdur,
    VMR, CI, PABw, IR;
cats = filter cats by subCat1 != 'unknown';

store cats into 'features_prec.out';

-- Calculate session metrics
sFeatures = M_SESSION_FEATURES(aFeatures);

store sFeatures into 'features_session.out';