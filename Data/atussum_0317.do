#delimit ;

* Edit the import statement to reference the data file on your computer.;

import delimited  
tucaseid
gemetsta
gtmetsta
peeduca
pehspnon
ptdtrace
teage
telfs
temjot
teschenr
teschlvl
tesex
tespempnot
trchildnum
trdpftpt
trernwa
trholiday
trspftpt
trsppres
tryhhchild
tudiaryday
tufnwgtp
tehruslt
tuyear
t010101
t010102
t010199
t010201
t010299
t010301
t010399
t010401
t010499
t010501
t010599
t019999
t020101
t020102
t020103
t020104
t020199
t020201
t020202
t020203
t020299
t020301
t020302
t020303
t020399
t020401
t020402
t020499
t020501
t020502
t020599
t020681
t020699
t020701
t020799
t020801
t020899
t020901
t020902
t020903
t020904
t020905
t020999
t029999
t030101
t030102
t030103
t030104
t030105
t030108
t030109
t030110
t030111
t030112
t030186
t030199
t030201
t030202
t030203
t030204
t030299
t030301
t030302
t030303
t030399
t030401
t030402
t030403
t030404
t030405
t030499
t030501
t030502
t030503
t030504
t030599
t039999
t040101
t040102
t040103
t040104
t040105
t040108
t040109
t040110
t040111
t040112
t040186
t040199
t040201
t040202
t040203
t040204
t040299
t040301
t040302
t040303
t040399
t040401
t040402
t040403
t040404
t040405
t040499
t040501
t040502
t040503
t040504
t040505
t040506
t040507
t040508
t040599
t049999
t050101
t050102
t050103
t050189
t050201
t050202
t050203
t050204
t050289
t050301
t050302
t050303
t050304
t050389
t050403
t050404
t050405
t050481
t050499
t059999
t060101
t060102
t060103
t060104
t060199
t060201
t060202
t060203
t060289
t060301
t060302
t060303
t060399
t060401
t060402
t060403
t060499
t069999
t070101
t070102
t070103
t070104
t070105
t070199
t070201
t070299
t070301
t070399
t079999
t080101
t080102
t080199
t080201
t080202
t080203
t080299
t080301
t080302
t080399
t080401
t080402
t080403
t080499
t080501
t080502
t080599
t080601
t080602
t080699
t080701
t080702
t080799
t080801
t080899
t089999
t090101
t090102
t090103
t090104
t090199
t090201
t090202
t090299
t090301
t090302
t090399
t090401
t090402
t090499
t090501
t090502
t090599
t099999
t100101
t100102
t100103
t100199
t100201
t100299
t100381
t100383
t100399
t100401
t100499
t109999
t110101
t110199
t110281
t110289
t119999
t120101
t120199
t120201
t120202
t120299
t120301
t120302
t120303
t120304
t120305
t120306
t120307
t120308
t120309
t120310
t120311
t120312
t120313
t120399
t120401
t120402
t120403
t120404
t120405
t120499
t120501
t120502
t120503
t120504
t120599
t129999
t130101
t130102
t130103
t130104
t130105
t130106
t130107
t130108
t130109
t130110
t130111
t130112
t130113
t130114
t130115
t130116
t130117
t130118
t130119
t130120
t130121
t130122
t130123
t130124
t130125
t130126
t130127
t130128
t130129
t130130
t130131
t130132
t130133
t130134
t130135
t130136
t130199
t130201
t130202
t130203
t130204
t130205
t130206
t130207
t130208
t130209
t130210
t130211
t130212
t130213
t130214
t130215
t130216
t130217
t130218
t130219
t130220
t130221
t130222
t130223
t130224
t130225
t130226
t130227
t130228
t130229
t130230
t130231
t130232
t130299
t130301
t130302
t130399
t130401
t130402
t130499
t139999
t140101
t140102
t140103
t140104
t140105
t149999
t150101
t150102
t150103
t150104
t150105
t150106
t150199
t150201
t150202
t150203
t150204
t150299
t150301
t150302
t150399
t150401
t150402
t150499
t150501
t150599
t150601
t150602
t150699
t159989
t160101
t160102
t160103
t160104
t160105
t160106
t160107
t160108
t169989
t180101
t180199
t180280
t180381
t180382
t180399
t180481
t180482
t180499
t180501
t180502
t180589
t180601
t180682
t180699
t180701
t180782
t180801
t180802
t180803
t180804
t180805
t180806
t180807
t180899
t180901
t180902
t180903
t180904
t180905
t180999
t181002
t181081
t181099
t181101
t181199
t181201
t181202
t181204
t181283
t181299
t181301
t181302
t181399
t181401
t181499
t181501
t181599
t181601
t181699
t181801
t181899
t189999
t500101
t500103
t500104
t500105
t500106
t500107
t509989
 using c:\atussum_0317.dat, stringcols(1) ;

label variable tucaseid "ATUS Case ID (14-digit identifier)";
label variable peeduca "Edited: highest level of school you have completed or highest degree received?";
label variable ptdtrace "Race (topcoded)";
label variable gtmetsta "Metropolitan status (2000 or 2010 definitions, see note)";
label variable pehspnon "Edited: are you Spanish, Hispanic, or Latino?";
label variable gemetsta "Metropolitan status (1990 definitions)";
label variable trchildnum "Number of household children < 18";
label variable tudiaryday "Day of the week of diary day (day about which the respondent was interviewed)";
label variable trernwa "Weekly earnings (2 implied decimals)";
label variable trholiday "Flag to indicate if diary day was a holiday";
label variable trspftpt "Full time or part time employment status of spouse or unmarried partner";
label variable trsppres "Presence of the respondent's spouse or unmarried partner in the household";
label variable trdpftpt "Full time or part time employment status of respondent";
label variable tufnwgtp "ATUS final weight";
label variable tespempnot "Edited: employment status of spouse or unmarried partner";
label variable teschlvl "Edited: would that be high school, college, or university?";
label variable teschenr "Edited: are you enrolled in high school, college, or university?";
label variable temjot "Edited: in the last seven days did you have more than one job?";
label variable telfs "Edited: labor force status";
label variable tehruslt "Edited: total hours usually worked per week (sum of TEHRUSL1 and TEHRUSL2)";
label variable tryhhchild "Age of youngest household child < 18";
label variable teage "Edited: age";
label variable tesex "Edited: sex";
label variable tuyear "Year of diary day (year of day about which respondent was interviewed)";
label variable t010101 "Sleeping";
label variable t010102 "Sleeplessness";
label variable t010199 "Sleeping, n.e.c.*";
label variable t010201 "Washing, dressing and grooming oneself";
label variable t010299 "Grooming, n.e.c.*";
label variable t010301 "Health-related self care";
label variable t010399 "Self care, n.e.c.*";
label variable t010401 "Personal/Private activities";
label variable t010499 "Personal activities, n.e.c.*";
label variable t010501 "Personal emergencies";
label variable t010599 "Personal care emergencies, n.e.c.*";
label variable t019999 "Personal care, n.e.c.*";
label variable t020101 "Interior cleaning";
label variable t020102 "Laundry";
label variable t020103 "Sewing, repairing, & maintaining textiles";
label variable t020104 "Storing interior hh items, inc. food";
label variable t020199 "Housework, n.e.c.* ";
label variable t020201 "Food and drink preparation ";
label variable t020202 "Food presentation";
label variable t020203 "Kitchen and food clean-up";
label variable t020299 "Food & drink prep, presentation, & clean-up, n.e.c.* ";
label variable t020301 "Interior arrangement, decoration, & repairs";
label variable t020302 "Building and repairing furniture";
label variable t020303 "Heating and cooling";
label variable t020399 "Interior maintenance, repair, & decoration, n.e.c.* ";
label variable t020401 "Exterior cleaning";
label variable t020402 "Exterior repair, improvements, & decoration";
label variable t020499 "Exterior maintenance, repair & decoration, n.e.c.*";
label variable t020501 "Lawn, garden, and houseplant care";
label variable t020502 "Ponds, pools, and hot tubs";
label variable t020599 "Lawn and garden, n.e.c.* ";
label variable t020681 "Care for animals and pets (not veterinary care)";
label variable t020699 "Pet and animal care, n.e.c.*";
label variable t020701 "Vehicle repair and maintenance (by self)";
label variable t020799 "Vehicles, n.e.c.*";
label variable t020801 "Appliance, tool, and toy set-up, repair, & maintenance (by self)";
label variable t020899 "Appliances and tools, n.e.c.*";
label variable t020901 "Financial management";
label variable t020902 "Household & personal organization and planning";
label variable t020903 "HH & personal mail & messages (except e-mail)";
label variable t020904 "HH & personal e-mail and messages";
label variable t020905 "Home security";
label variable t020999 "Household management, n.e.c.*";
label variable t029999 "Household activities, n.e.c.*";
label variable t030101 "Physical care for hh children";
label variable t030102 "Reading to/with hh children";
label variable t030103 "Playing with hh children, not sports";
label variable t030104 "Arts and crafts with hh children";
label variable t030105 "Playing sports with hh children";
label variable t030186 "Talking with/listening to hh children";
label variable t030108 "Organization & planning for hh children";
label variable t030109 "Looking after hh children (as a primary activity)";
label variable t030110 "Attending hh children's events";
label variable t030111 "Waiting for/with hh children";
label variable t030112 "Picking up/dropping off hh children";
label variable t030199 "Caring for & helping hh children, n.e.c.*";
label variable t030201 "Homework (hh children)";
label variable t030202 "Meetings and school conferences (hh children)";
label variable t030203 "Home schooling of hh children";
label variable t030204 "Waiting associated with hh children's education";
label variable t030299 "Activities related to hh child's education, n.e.c.*";
label variable t030301 "Providing medical care to hh children";
label variable t030302 "Obtaining medical care for hh children";
label variable t030303 "Waiting associated with hh children's health";
label variable t030399 "Activities related to hh child's health, n.e.c.*";
label variable t030401 "Physical care for hh adults";
label variable t030402 "Looking after hh adult (as a primary activity)";
label variable t030403 "Providing medical care to hh adult";
label variable t030404 "Obtaining medical and care services for hh adult";
label variable t030405 "Waiting associated with caring for household adults";
label variable t030499 "Caring for household adults, n.e.c.* ";
label variable t030501 "Helping hh adults";
label variable t030502 "Organization & planning for hh adults";
label variable t030503 "Picking up/dropping off hh adult";
label variable t030504 "Waiting associated with helping hh adults";
label variable t030599 "Helping household adults, n.e.c.*";
label variable t039999 "Caring for & helping hh members, n.e.c.*";
label variable t040101 "Physical care for nonhh children";
label variable t040102 "Reading to/with nonhh children";
label variable t040103 "Playing with nonhh children, not sports";
label variable t040104 "Arts and crafts with nonhh children";
label variable t040105 "Playing sports with nonhh children";
label variable t040186 "Talking with/listening to nonhh children";
label variable t040108 "Organization & planning for nonhh children";
label variable t040109 "Looking after nonhh children (as primary activity)";
label variable t040110 "Attending nonhh children's events";
label variable t040111 "Waiting for/with nonhh children";
label variable t040112 "Dropping off/picking up nonhh children";
label variable t040199 "Caring for and helping nonhh children, n.e.c.*";
label variable t040201 "Homework (nonhh children)";
label variable t040202 "Meetings and school conferences (nonhh children)";
label variable t040203 "Home schooling of nonhh children";
label variable t040204 "Waiting associated with nonhh children's education";
label variable t040299 "Activities related to nonhh child's educ., n.e.c.*";
label variable t040301 "Providing medical care to nonhh children";
label variable t040302 "Obtaining medical care for nonhh children";
label variable t040303 "Waiting associated with nonhh children's health";
label variable t040399 "Activities related to nonhh child's health, n.e.c.*";
label variable t040401 "Physical care for nonhh adults";
label variable t040402 "Looking after nonhh adult (as a primary activity)";
label variable t040403 "Providing medical care to nonhh adult";
label variable t040404 "Obtaining medical and care services for nonhh adult";
label variable t040405 "Waiting associated with caring for nonhh adults";
label variable t040499 "Caring for nonhh adults, n.e.c.*";
label variable t040501 "Housework, cooking, & shopping assistance for nonhh adults";
label variable t040502 "House & lawn maintenance & repair assistance for nonhh adults";
label variable t040503 "Animal & pet care assistance for nonhh adults";
label variable t040504 "Vehicle & appliance maintenance/repair assistance for nonhh adults";
label variable t040505 "Financial management assistance for nonhh adults";
label variable t040506 "Household management & paperwork assistance for nonhh adults";
label variable t040507 "Picking up/dropping off nonhh adult";
label variable t040508 "Waiting associated with helping nonhh adults";
label variable t040599 "Helping nonhh adults, n.e.c.*";
label variable t049999 "Caring for & helping nonhh members, n.e.c.*";
label variable t050101 "Work, main job";
label variable t050102 "Work, other job(s)";
label variable t050103 "Security procedures related to work";
label variable t050189 "Working, n.e.c.*";
label variable t050201 "Socializing, relaxing, and leisure as part of job";
label variable t050202 "Eating and drinking as part of job";
label variable t050203 "Sports and exercise as part of job";
label variable t050204 "Security procedures as part of job";
label variable t050289 "Work-related activities, n.e.c.*";
label variable t050301 "Income-generating hobbies, crafts, and food";
label variable t050302 "Income-generating performances ";
label variable t050303 "Income-generating services ";
label variable t050304 "Income-generating rental property activities";
label variable t050389 "Other income-generating activities, n.e.c.*";
label variable t050481 "Job search activities";
label variable t050403 "Job interviewing ";
label variable t050404 "Waiting associated with job search or interview";
label variable t050405 "Security procedures rel. to job search/interviewing";
label variable t050499 "Job search and Interviewing, n.e.c.*";
label variable t059999 "Work and work-related activities, n.e.c.*";
label variable t060101 "Taking class for degree, certification, or licensure";
label variable t060102 "Taking class for personal interest";
label variable t060103 "Waiting associated with taking classes";
label variable t060104 "Security procedures rel. to taking classes";
label variable t060199 "Taking class, n.e.c.* ";
label variable t060201 "Extracurricular club activities";
label variable t060202 "Extracurricular music & performance activities";
label variable t060203 "Extracurricular student government activities";
label variable t060289 "Education-related extracurricular activities, n.e.c.*";
label variable t060301 "Research/homework for class for degree, certification, or licensure";
label variable t060302 "Research/homework for class for pers. interest";
label variable t060303 "Waiting associated with research/homework";
label variable t060399 "Research/homework n.e.c.*";
label variable t060401 "Administrative activities: class for degree, certification, or licensure";
label variable t060402 "Administrative activities: class for personal interest";
label variable t060403 "Waiting associated w/admin. activities (education)";
label variable t060499 "Administrative for education, n.e.c.*";
label variable t069999 "Education, n.e.c.*";
label variable t070101 "Grocery shopping";
label variable t070102 "Purchasing gas";
label variable t070103 "Purchasing food (not groceries)";
label variable t070104 "Shopping, except groceries, food and gas";
label variable t070105 "Waiting associated with shopping";
label variable t070199 "Shopping, n.e.c.*";
label variable t070201 "Comparison shopping";
label variable t070299 "Researching purchases, n.e.c.*";
label variable t070301 "Security procedures rel. to consumer purchases";
label variable t070399 "Security procedures rel. to consumer purchases, n.e.c.*";
label variable t079999 "Consumer purchases, n.e.c.*";
label variable t080101 "Using paid childcare services";
label variable t080102 "Waiting associated w/purchasing childcare svcs";
label variable t080199 "Using paid childcare services, n.e.c.*";
label variable t080201 "Banking";
label variable t080202 "Using other financial services";
label variable t080203 "Waiting associated w/banking/financial services";
label variable t080299 "Using financial services and banking, n.e.c.*";
label variable t080301 "Using legal services";
label variable t080302 "Waiting associated with legal services";
label variable t080399 "Using legal services, n.e.c.*";
label variable t080401 "Using health and care services outside the home";
label variable t080402 "Using in-home health and care services";
label variable t080403 "Waiting associated with medical services";
label variable t080499 "Using medical services, n.e.c.*";
label variable t080501 "Using personal care services";
label variable t080502 "Waiting associated w/personal care services";
label variable t080599 "Using personal care services, n.e.c.*";
label variable t080601 "Activities rel. to purchasing/selling real estate";
label variable t080602 "Waiting associated w/purchasing/selling real estate";
label variable t080699 "Using real estate services, n.e.c.*";
label variable t080701 "Using veterinary services";
label variable t080702 "Waiting associated with veterinary services";
label variable t080799 "Using veterinary services, n.e.c.*";
label variable t080801 "Security procedures rel. to professional/personal svcs.";
label variable t080899 "Security procedures rel. to professional/personal svcs n.e.c.*";
label variable t089999 "Professional and personal services, n.e.c.*";
label variable t090101 "Using interior cleaning services ";
label variable t090102 "Using meal preparation services";
label variable t090103 "Using clothing repair and cleaning services";
label variable t090104 "Waiting associated with using household services";
label variable t090199 "Using household services, n.e.c.*";
label variable t090201 "Using home maint/repair/decor/construction svcs";
label variable t090202 "Waiting associated w/ home main/repair/decor/constr";
label variable t090299 "Using home maint/repair/decor/constr services, n.e.c.*";
label variable t090301 "Using pet services";
label variable t090302 "Waiting associated with pet services";
label variable t090399 "Using pet services, n.e.c.*";
label variable t090401 "Using lawn and garden services";
label variable t090402 "Waiting associated with using lawn & garden services";
label variable t090499 "Using lawn and garden services, n.e.c.*";
label variable t090501 "Using vehicle maintenance or repair services";
label variable t090502 "Waiting associated with vehicle main. or repair svcs";
label variable t090599 "Using vehicle maint. & repair svcs, n.e.c.*";
label variable t099999 "Using household services, n.e.c.*";
label variable t100101 "Using police and fire services";
label variable t100102 "Using social services";
label variable t100103 "Obtaining licenses & paying fines, fees, taxes";
label variable t100199 "Using government services, n.e.c.*";
label variable t100201 "Civic obligations & participation";
label variable t100299 "Civic obligations & participation, n.e.c.*";
label variable t100381 "Waiting associated w/ using government services";
label variable t100383 "Waiting associated w/civic obligations & participation";
label variable t100399 "Waiting assoc. w/govt svcs or civic obligations, n.e.c.*";
label variable t100401 "Security procedures rel. to govt svcs/civic obligations";
label variable t100499 "Security procedures rel. to govt svcs/civic obligations, n.e.c.*";
label variable t109999 "Government services, n.e.c.*";
label variable t110101 "Eating and drinking";
label variable t110199 "Eating and drinking, n.e.c.*";
label variable t110281 "Waiting associated w/eating & drinking";
label variable t110289 "Waiting associated with eating & drinking, n.e.c.*";
label variable t119999 "Eating and drinking, n.e.c.*";
label variable t120101 "Socializing and communicating with others";
label variable t120199 "Socializing and communicating, n.e.c.*";
label variable t120201 "Attending or hosting parties/receptions/ceremonies";
label variable t120202 "Attending meetings for personal interest (not volunteering)";
label variable t120299 "Attending/hosting social events, n.e.c.*";
label variable t120301 "Relaxing, thinking ";
label variable t120302 "Tobacco and drug use";
label variable t120303 "Television and movies (not religious)";
label variable t120304 "Television (religious)";
label variable t120305 "Listening to the radio";
label variable t120306 "Listening to/playing music (not radio)";
label variable t120307 "Playing games";
label variable t120308 "Computer use for leisure (exc. Games)";
label variable t120309 "Arts and crafts as a hobby";
label variable t120310 "Collecting as a hobby";
label variable t120311 "Hobbies, except arts & crafts and collecting";
label variable t120312 "Reading for personal interest";
label variable t120313 "Writing for personal interest ";
label variable t120399 "Relaxing and leisure, n.e.c.*";
label variable t120401 "Attending performing arts";
label variable t120402 "Attending museums";
label variable t120403 "Attending movies/film";
label variable t120404 "Attending gambling establishments";
label variable t120405 "Security procedures rel. to arts & entertainment";
label variable t120499 "Arts and entertainment, n.e.c.*";
label variable t120501 "Waiting assoc. w/socializing & communicating";
label variable t120502 "Waiting assoc. w/attending/hosting social events";
label variable t120503 "Waiting associated with relaxing/leisure";
label variable t120504 "Waiting associated with arts & entertainment";
label variable t120599 "Waiting associated with socializing, n.e.c.*";
label variable t129999 "Socializing, relaxing, and leisure, n.e.c.*";
label variable t130101 "Doing aerobics";
label variable t130102 "Playing baseball";
label variable t130103 "Playing basketball";
label variable t130104 "Biking";
label variable t130105 "Playing billiards";
label variable t130106 "Boating";
label variable t130107 "Bowling ";
label variable t130108 "Climbing, spelunking, caving";
label variable t130109 "Dancing";
label variable t130110 "Participating in equestrian sports";
label variable t130111 "Fencing";
label variable t130112 "Fishing";
label variable t130113 "Playing football";
label variable t130114 "Golfing";
label variable t130115 "Doing gymnastics";
label variable t130116 "Hiking";
label variable t130117 "Playing hockey";
label variable t130118 "Hunting";
label variable t130119 "Participating in martial arts";
label variable t130120 "Playing racquet sports ";
label variable t130121 "Participating in rodeo competitions";
label variable t130122 "Rollerblading";
label variable t130123 "Playing rugby";
label variable t130124 "Running";
label variable t130125 "Skiing, ice skating, snowboarding";
label variable t130126 "Playing soccer";
label variable t130127 "Softball";
label variable t130128 "Using cardiovascular equipment";
label variable t130129 "Vehicle touring/racing";
label variable t130130 "Playing volleyball";
label variable t130131 "Walking";
label variable t130132 "Participating in water sports";
label variable t130133 "Weightlifting/strength training";
label variable t130134 "Working out, unspecified";
label variable t130135 "Wrestling";
label variable t130136 "Doing yoga";
label variable t130199 "Playing sports n.e.c.*";
label variable t130201 "Watching aerobics";
label variable t130202 "Watching baseball";
label variable t130203 "Watching basketball";
label variable t130204 "Watching biking";
label variable t130205 "Watching billiards";
label variable t130206 "Watching boating";
label variable t130207 "Watching bowling";
label variable t130208 "Watching climbing, spelunking, caving";
label variable t130209 "Watching dancing";
label variable t130210 "Watching equestrian sports";
label variable t130211 "Watching fencing";
label variable t130212 "Watching fishing";
label variable t130213 "Watching football";
label variable t130214 "Watching golfing";
label variable t130215 "Watching gymnastics";
label variable t130216 "Watching hockey";
label variable t130217 "Watching martial arts ";
label variable t130218 "Watching racquet sports";
label variable t130219 "Watching rodeo competitions";
label variable t130220 "Watching rollerblading";
label variable t130221 "Watching rugby";
label variable t130222 "Watching running";
label variable t130223 "Watching skiing, ice skating, snowboarding";
label variable t130224 "Watching soccer";
label variable t130225 "Watching softball";
label variable t130226 "Watching vehicle touring/racing";
label variable t130227 "Watching volleyball";
label variable t130228 "Watching walking";
label variable t130229 "Watching water sports";
label variable t130230 "Watching weightlifting/strength training";
label variable t130231 "Watching people working out, unspecified";
label variable t130232 "Watching wrestling";
label variable t130299 "Attending sporting events, n.e.c.*";
label variable t130301 "Waiting related to playing sports or exercising";
label variable t130302 "Waiting related to attending sporting events";
label variable t130399 "Waiting associated with sports, exercise, & recreation, n.e.c.*";
label variable t130401 "Security related to playing sports or exercising";
label variable t130402 "Security related to attending sporting events";
label variable t130499 "Security related to sports, exercise, & recreation, n.e.c.*";
label variable t139999 "Sports, exercise, & recreation, n.e.c.*";
label variable t140101 "Attending religious services";
label variable t140102 "Participation in religious practices";
label variable t140103 "Waiting associated w/religious & spiritual activities";
label variable t140104 "Security procedures rel. to religious & spiritual activities";
label variable t140105 "Religious education activities";
label variable t149999 "Religious and spiritual activities, n.e.c.*";
label variable t150101 "Computer use";
label variable t150102 "Organizing and preparing";
label variable t150103 "Reading";
label variable t150104 "Telephone calls (except hotline counseling)";
label variable t150105 "Writing";
label variable t150106 "Fundraising";
label variable t150199 "Administrative & support activities, n.e.c.*";
label variable t150201 "Food preparation, presentation, clean-up";
label variable t150202 "Collecting & delivering clothing & other goods";
label variable t150203 "Providing care";
label variable t150204 "Teaching, leading, counseling, mentoring";
label variable t150299 "Social service & care activities, n.e.c.*";
label variable t150301 "Building houses, wildlife sites, & other structures";
label variable t150302 "Indoor & outdoor maintenance, repair, & clean-up";
label variable t150399 "Indoor & outdoor maintenance, building & clean-up activities, n.e.c.*";
label variable t150401 "Performing";
label variable t150402 "Serving at volunteer events & cultural activities";
label variable t150499 "Participating in performance & cultural activities, n.e.c.*";
label variable t150501 "Attending meetings, conferences, & training";
label variable t150599 "Attending meetings, conferences, & training, n.e.c.*";
label variable t150601 "Public health activities";
label variable t150602 "Public safety activities";
label variable t150699 "Public health & safety activities, n.e.c.*";
label variable t159989 "Volunteer activities, n.e.c.*";
label variable t160101 "Telephone calls to/from family members";
label variable t160102 "Telephone calls to/from friends, neighbors, or acquaintances";
label variable t160103 "Telephone calls to/from education services providers";
label variable t160104 "Telephone calls to/from salespeople";
label variable t160105 "Telephone calls to/from professional or personal care svcs providers";
label variable t160106 "Telephone calls to/from household services providers";
label variable t160107 "Telephone calls to/from paid child or adult care providers";
label variable t160108 "Telephone calls to/from government officials";
label variable t169989 "Telephone calls, n.e.c.*";
label variable t180101 "Travel related to personal care";
label variable t180199 "Travel related to personal care, n.e.c.*";
label variable t180280 "Travel related to household activities   ";
label variable t180381 "Travel related to caring for and helping hh children";
label variable t180382 "Travel related to caring for and helping hh adults";
label variable t180399 "Travel rel. to caring for & helping hh members, n.e.c.*";
label variable t180481 "Travel related to caring for and helping nonhh children";
label variable t180482 "Travel related to caring for and helping nonhh adults";
label variable t180499 "Travel rel. to caring for & helping nonhh members, n.e.c.*";
label variable t180501 "Travel related to working";
label variable t180502 "Travel related to work-related activities";
label variable t180589 "Travel related to work, n.e.c.*";
label variable t180601 "Travel related to taking class";
label variable t180682 "Travel related to education (except taking class)";
label variable t180699 "Travel related to education, n.e.c.*";
label variable t180701 "Travel related to grocery shopping";
label variable t180782 "Travel related to shopping (except grocery shopping)";
label variable t180801 "Travel related to using childcare services";
label variable t180802 "Travel related to using financial services and banking";
label variable t180803 "Travel related to using legal services";
label variable t180804 "Travel related to using medical services";
label variable t180805 "Travel related to using personal care services";
label variable t180806 "Travel related to using real estate services";
label variable t180807 "Travel related to using veterinary services";
label variable t180899 "Travel rel. to using prof. & personal care services, n.e.c.*";
label variable t180901 "Travel related to using household services";
label variable t180902 "Travel related to using home main./repair/decor./construction svcs";
label variable t180903 "Travel related to using pet services (not vet)";
label variable t180904 "Travel related to using lawn and garden services";
label variable t180905 "Travel related to using vehicle maintenance & repair services";
label variable t180999 "Travel related to using household services, n.e.c.*";
label variable t181081 "Travel related to using government services";
label variable t181002 "Travel related to civic obligations & participation";
label variable t181099 "Travel rel. to govt svcs & civic obligations, n.e.c.*";
label variable t181101 "Travel related to eating and drinking";
label variable t181199 "Travel related to eating and drinking, n.e.c.*";
label variable t181201 "Travel related to socializing and communicating";
label variable t181202 "Travel related to attending or hosting social events";
label variable t181283 "Travel related to relaxing and leisure";
label variable t181204 "Travel related to arts and entertainment";
label variable t181299 "Travel rel. to socializing, relaxing, & leisure, n.e.c.*";
label variable t181301 "Travel related to participating in sports/exercise/recreation";
label variable t181302 "Travel related to attending sporting/recreational events";
label variable t181399 "Travel related to sports, exercise, & recreation, n.e.c.*";
label variable t181401 "Travel related to religious/spiritual practices";
label variable t181499 "Travel rel. to religious/spiritual activities, n.e.c.*";
label variable t181501 "Travel related to volunteering";
label variable t181599 "Travel related to volunteer activities, n.e.c.*";
label variable t181601 "Travel related to phone calls";
label variable t181699 "Travel rel. to phone calls, n.e.c.*";
label variable t181801 "Security procedures related to traveling";
label variable t181899 "Security procedures related to traveling, n.e.c.*";
label variable t189999 "Traveling, n.e.c.*";
label variable t500101 "Insufficient detail in verbatim";
label variable t500103 "Missing travel or destination";
label variable t500104 "Recorded simultaneous activities incorrectly";
label variable t500105 "Respondent refused to provide information/none of your business";
label variable t500106 "Gap/can't remember";
label variable t500107 "Unable to code activity at 1st tier";
label variable t509989 "Data codes, n.e.c.*";

capture label define labelgemetsta
-1 "Blank"
-2 "Don't Know"
-3 "Refused"
1 "Metropolitan"
2 "Non-metropolitan"
3 "Not identified"
;
capture label define labelgtmetsta
-1 "Blank"
-2 "Don't Know"
-3 "Refused"
1 "Metropolitan"
2 "Non-metropolitan"
3 "Not identified"
;
capture label define labelpeeduca
31 "Less than 1st grade"
32 "1st, 2nd, 3rd, or 4th grade"
33 "5th or 6th grade"
34 "7th or 8th grade"
35 "9th grade"
36 "10th grade"
37 "11th grade"
38 "12th grade - no diploma"
39 "High school graduate - diploma or equivalent (GED)"
40 "Some college but no degree"
41 "Associate degree - occupational/vocational"
42 "Associate degree - academic program"
43 "Bachelor's degree (BA, AB, BS, etc.)"
44 "Master's degree (MA, MS, MEng, MEd, MSW, etc.)"
45 "Professional school degree (MD, DDS, DVM, etc.)"
46 "Doctoral degree (PhD, EdD, etc.)"
;
capture label define labelpehspnon
1 "Hispanic"
2 "Non-Hispanic"
;
capture label define labelptdtrace
1 "White only"
2 "Black only"
3 "American Indian, Alaskan Native only"
4 "Asian only"
5 "Hawaiian/Pacific Islander only"
6 "White-Black"
7 "White-American Indian"
8 "White-Asian"
9 "White-Hawaiian"
10 "Black-American Indian"
11 "Black-Asian"
12 "Black-Hawaiian"
13 "American Indian-Asian"
14 "Asian-Hawaiian"
15 "White-Black-American Indian"
16 "White-Black-Asian"
17 "White-American Indian-Asian"
18 "White-Asian-Hawaiian"
19 "White-Black-American Indian-Asian"
20 "2 or 3 races"
21 "4 or 5 races"
;
capture label define labeltelfs
1 "Employed - at work"
2 "Employed - absent"
3 "Unemployed - on layoff"
4 "Unemployed - looking"
5 "Not in labor force"
;
capture label define labeltemjot
1 "Yes"
2 "No"
;
capture label define labelteschenr
1 "Yes"
2 "No"
;
capture label define labelteschlvl
1 "High school"
2 "College or university"
;
capture label define labeltespempnot
1 "Employed"
2 "Not employed"
;
capture label define labeltrdpftpt
1 "Full time"
2 "Part time"
;
capture label define labeltrholiday
0 "Diary day was not a holiday"
1 "Diary day was a holiday"
;
capture label define labeltrspftpt
1 "Full time"
2 "Part time"
3 "Hours vary"
;
capture label define labeltrsppres
1 "Spouse present"
2 "Unmarried partner present"
3 "No spouse or unmarried partner present"
;
capture label define labeltudiaryday
1 "Sunday"
2 "Monday"
3 "Tuesday"
4 "Wednesday"
5 "Thursday"
6 "Friday"
7 "Saturday"
;
capture label define labeltesex
1 "Male"
2 "Female"
;

label values gemetsta   labelgemetsta;
label values gtmetsta   labelgtmetsta;
label values peeduca    labelpeeduca;
label values pehspnon   labelpehspnon;
label values ptdtrace   labelptdtrace;
label values telfs      labeltelfs;
label values temjot     labeltemjot;
label values teschenr   labelteschenr;
label values teschlvl   labelteschlvl;
label values tespempnot labeltespempnot;
label values trdpftpt   labeltrdpftpt;
label values trholiday  labeltrholiday;
label values trspftpt   labeltrspftpt;
label values trsppres   labeltrsppres;
label values tudiaryday labeltudiaryday;
label values tesex      labeltesex;

describe, short;
