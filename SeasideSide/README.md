# Data Dictionary

## File Structure

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

[Location Name]\_[Date (Format: DD.MM.YYYY)]\_[Form Type (Pre/Post)]\_[Event Type (Participants/Volunteers)]

Examples:

* Bondi Junction_03.02.2021_Pre_Participants

    * In this example, use a space if there are two words in the location. Make sure you do not use an underscore in the location name (e.g. Bondi_Junction_03.02.2021_Pre_Participants)

String functions for extracting information:

* Locations can be extracted by filtering up to first number
* Date can be extracted since they are the only numbers in the file name
* Data type can be determined by doing a search of the two words on the string

