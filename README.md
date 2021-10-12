# Background
Shiny App data analytics platform built for a 180 Degrees Consulting project. 
Key features are;
- Personalised data wrangling to interpret Google Forms and Eventbrite information, including some flexibility for form type
- Customisable visualisations, separated into event, year, and location views
- Ability to export reports for event and yearly summaries

## Folder Structure
1. SeasideSide: R/HTML/CSS Code for app [Sourish & Jennifer]
2. Data Wrangling Functions: Transforms excel input into useful dataframes [Jennifer & Lalitha]
3. Visualisation Functions: Transforms wrangled dataframes into visualisations [Julia & Mukund]
4. Reports: R Markdowns for export report functionality
5. Testing Datasets: Dummy datasets

# Input requirements

## Excel Input Requirements 
This app requires specific excel data inputs in order to function as intended.

### Pre-Event Excel requires the following *exact* headings:
* Email
* What pronoun do you identify with?
* Age
* Postcode<br/><br/>
_Age Range Questions_
* How many people under (5) years are you signing up for?
* How many people in the age range(11-20) are you signing up for?
* How many people in the age range(21-30) are you signing up for?
* How many people in the age range(31-50) are you signing up for?
* How many people in the age range(51-70) are you signing up for?
* How many people in the age range(71+) are you signing up for?
* Have you previously attended an organised clean-up event (hosted by any organisation)?

* On a scale of 1-5 how invested are you in environmental impact?(1 being the least invested)
* How did you find out about this event?
* Are you of Aboriginal or Torres Strait Islander Origin?

### Post-Event Excel has more flexibility, but requires the following:
* To calculate a specific KPI, you **must** add the word "community", "learning", or "action" into the excel heading in order for it to be interpreted correctly.
* Community and learning KPI questions must be in the format of:
  * Question with options which are proposed learning/community outcomes
  * Question must also include the total number of options (i.e. Select all **3** that apply)
* Action KPI questions must be in the format of:
  * Question with sub questions, with the **exact** options of ("Doing before the event", "Started doing after the event", "Likely to do in the next 3 months", "Unlikely to do in the next 3 months")
* Also the extra question "On a scale of 1-5, how would you rate your environmental habits after the activity?"

* **Note:** With the current post-event survey format, it would be required to insert "action" into each heading of the excel spreadsheet manually, whereas the "learning" and "community" questions already have the headings embedded. 

### Key Notes:
* Ensure to not re-use the words "community", "action", or "learning" in the incorrect context, as this would be misinterpreted as a different KPI.
* Any differences in post-event questions between years will not affect the calculations.
* Community & Learning KPIs are calculated for each individual as proportion of the number of responses they have chosen against the total number of options are available. The overall KPI of a dataset is given as the mean of all individuals.
* Action KPIs are calculated similarly, except a response is recorded positively if a participant is "likely to do..." or "Started doing..." after the event. 


## Google Drive File Structure
* Pre
  * 2018
  * 2019
  * 2020
  * 2021
* Post
  * 2018
  * 2019
  * 2020
  * 2021

## File Naming Convention
[Location Name]_[Postcode]_[Date (Format: DD.MM.YYYY)]_[Form Type (Pre/Post)]_[Event Type (Participants/Volunteers)]

Examples:
* Bondi Junction_2022_03.02.2021_Pre_Participants
  * In this example, use a space if there are two words in the location. Make sure you do not use an underscore in the location name (e.g. Bondi_Junction_2022_03.02.2021_Pre_Participants)

Important things to note:
* The underscore is incredibly important in separating the different fields in this file name. It shouldn't be used for any other purpose.
* Extra fields can be potentially added

## Credits
Built by University of Sydney Students: Sourish Iyengar, Jennifer Chen, Allan Wu, Lalitha Vadlamani, Julia Page, Mukund Karthik, Kevin Hou
