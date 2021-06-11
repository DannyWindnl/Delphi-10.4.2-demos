# Delphi-10.4.2-demos
Demos for the online Delphi 10.4.2 presentation of March 18th 2021 

 The online video for this presentation showcased these two Delphi 10.4.2 demo examples.
 
 1. Demo for the new Delphi 10.4.2 TNumberBox control
    Requests the current Euro exchange rates from thge European Central Bank and displays these in TNumberBox components. TNumberBox overrides TFormatSettings loaded from the system if set in the
 TNumberBox properties. e.g TFormatSettings.CurrencyFormat; CurrencyFormat - Defines the currency symbol placement and separation used in floating-point to decimal conversions. 
 
 Possible values are:

    0 = '$1'
    1 = '1$'
    2 = '$ 1'
    3 = '1 $'

 2. Demo for Android 10/11 Permissions
    Required starting from November-2020 due to minimal target SDK requirement 29 by Google. The permissions in the ComboBox are the default permissions that are used when starting development on an Android App (debug mode).
    Based on example code which is bundled in Delphi 10.4.2
 
 Author: Danny Wind
 License: Creative Commons CC-BY
