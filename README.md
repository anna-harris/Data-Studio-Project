# Restoration and Parolee Voting Behavior in New Jersey


Instructions

Raw data can be accessed by completing public records requests with the New Jersey Department of Corrections, New Jersey State Parole Board, and the Department of State's Division of Elections for the following timeframe 2016 - June 2022. 

To make a request, go to the following website:

https://www-njlib.state.nj.us/NJ_OPRA/department.jsp

For the Department of Corrections:

From the drop down choose Corrections and fill out the fields. Make the following request in the message body:

Inmate release data from 01/01/2016 - 05/31/2022. Please include the following fields:
Offender name (First & Last), SBIH, DOB, Race/Ethnicity (if available), Release Date, Offense Type (Misdemeanor, Felony, etc), Reason For Release (deceased, released to the community, parole, etc), start date of supervision (start date of parole if applicable), end date of supervision (end date of parole if applicable), & County of Commitment

For the Department of State Elections Division:

From the drop down choose State, in the next drop down choose Division of Elections and fill out the fields. Make the following request in the message body:

Voter files that includes name, dob, address, and whether the person voted in any election between 01/01/2016 - 05/31/2022. 

For the State Parole Board:

From the drop down choose Commissions and Agencies, in the next drop down choose State Parole Board and fill out the fields. Make the following request in the message body:

Parolee files from 01/01/2016 - 05/31/2022 that includes name, dob, sbi, supervision status, parole start date, and parole end date. 

Once you have the data, start your analysis by working with the voter files since they take the longest among of time to load. To make life easier be sure to stick to the code and only include records with voter registration dates from after 2015. Also, clear your environment after loading each county. This will save your computer memory and make the analysis go faster. 

From there, follow the code and load the 21 csvs into a list and load up the data. 

As you run the code, be aware that only the first visualization uses R. You can prep by opening Excel and Google Slides in advance and refamiliarzing yourself with these platforms if you are rusty.

Additionally, given that the data contains personal information take care to store your files securely and make sure you remove all unique identifiers from your code before posting, sharing with others, etc. 

Last, you will notice that my script doesn't contain anything for fuzzy matching. That's because I found less than 200 fuzzy matches and figured that excluding them would save time without hurting the analysis. 

Thanks for reading and have fun with your own explorations as I've only just scratched the surface!
