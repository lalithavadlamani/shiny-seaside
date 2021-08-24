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

String functions for extracting information:

* Locations can be extracted by filtering up to first number
* Date can be extracted since they are the only numbers in the file name
* Data type can be determined by doing a search of the two words on the string

