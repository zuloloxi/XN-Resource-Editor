Colin Wilson's Delphi Components for Delphi 2009
================================================

1.  To install these components...

Copy the bpl files to your package output directory
Copy the dcp files to your DCP output directory

Install the design-time packages.  Select Component, Install Packages, click the 
'Add...' button, and choose 
  MiscComponentsD120.bpl, 
  MMCExpert120.bpl,
  PropertyPageForms120.bpl and 
  VirtualTreesExtensions120.bpl


2.  Installing the source code.

Copy all the files in the Source directory to a directory that's in your source path.  
It helps if its in your browsing path too.


3.  Instructions...

MMC expert
----------
To use the MMC snapin expert, select File, New, Other and choose 'MMC Snapin Project'
from the Projects tab.  Work through the simple wizard, then save all and compile the 
project.

To install the snapin in vista, open a command prompt as administrator.  Navigvate to the
directory where the snapin dll was created and type 'regsvr32 yoursnapin.dll'.  Note you
must be running as administrator for - otherwise you get an error.

Once the snapin's installed, you can try it out by running mmc, selecting add/remove snapin
from the file menu, and choosing your snapin from the list.


Property Page Forms
-------------------
The property page forms allow you to create 'settings' windows like you get when you do
Tools, Options in XanaNews, or in the Delph IDE.

The property page forms can be accessed from File, New, Other, Delphi Files, and select 
Property Base Form or Property Page Form.  Note that you should always select 'Inherit', rather
than Use or Copy.  Typically your project might inherit a single property base form, and 
several property page forms.

They are powerful, but quite complicated to set up initially.  I've provided a very simple
project with source in the Demos folder to show how its done.


Components
----------
The components will appear in the 'Colin Wilson's Delphi Components' tab in the IDE.  
Some highlights...

* TColorSelector is the color selector I use in XN Resource Editor

* TCountryComboBox is a combo-box that allows you to select a country.  You can choose a 
  particular county or countries to appear at the top of the list

* TCWRichEdit is a wrapper for the Windows common controls RICHEDIT control.  Like the TRichEdit
  that comes with Delphi but much more powerful.  I use a derivitave of it in XanaNews to
  display the text (see also TNewsRichEdit)

* TExWebBrowser is a wrapper for the windows web browser control - much more powerful than
  Delphi's native browser.

* TRegistryPersistentOptions, TXMLPersistentOptions and TInIFilePersistentOptions let you
  persist options in the registry, XML or INI file.


Colin Wilson.  September 2008
