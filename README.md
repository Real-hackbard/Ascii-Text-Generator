# Ascii-Text-Generator:

```ruby
Compiler    : Delphi10 Seattle, 10.1 Berlin, 10.2 Tokyo, 10.3 Rio, 10.4 Sydney, 11 Alexandria, 12 Athens
Components  : PNGImage.pas, FIGlet.pas, LabelLien.pas
Discription : ASCii Text Art Creator
Last Update : 08/2025
License     : Freeware
```


### 1.  What is FIGlet? 
### 2.  Where can I get FIGlet from?
### 3.  Where can I get more fonts for FIGlet?
### 4.  How can I contribute?
### 5.  Where does the name FIGlet come from?

Once you've installed the PNG components and successfully compiled the project, you need to download the fonts. Create a "Font" folder in the project folder and copy the *.fif files into this folder to render them.

Font Download : http://www.figlet.org/fontdb.cgi


A FIGlet font is a decorative font composed of ASCII characters to display letters and numbers. FIGlet fonts are part of ASCII art.
The name originally comes from a computer program called FIGlet (an acronym for Frank, Ian, and Glen's letters), which uses these fonts to create ASCII art.
With the ASCii Text Creator, you can create FIGlet fonts at the touch of a button. There are 302 font arts available to choose from.
The "Fonts" folder can also be expanded with your own FIGlet fonts if desired. Simply copy the files into the folder; they should have the *.fif file extension so that the tool recognizes them.

![Ascii Text Generator 1 0 Source](https://github.com/user-attachments/assets/4a9b97e1-85b0-41f3-ba6c-479c939097e0)

The tool can also export ASCII text in images, offering several features. RGB color values are adjustable, and a gradient can be generated.

Posible Export formats: *.BMP; *.JPG; *.JPEG; *.PNG; *.GIF; *.TIF; *.WMF; *.EMF; *.PSD; *.PDF;

FIGlet prints its input with large characters (called "FIGcharacters") composed of ordinary screen characters (so-called "sub-characters"). FIGlet output is generally reminiscent of the kind of "sign-natures" that many people like to put at the end of email and UseNet messages. It is also reminiscent of the output of some banner programs, although it is normally aligned, not sideways.

FIGULET can print in a variety of fonts, both left-to-right and right-to-left, with adjacent FIG characters kerned and squashed together in various ways.
FIGULET fonts are stored in separate files, which can be denoted by the suffix ".flf." Most FIGlet font files are stored in FIGlet's standard fonts directory.

The file format for FIGlet fonts is specified in the FIGfont Version 2 FIGfont and FIGdriver standard. The fonts are stored in ASCII files with the filename extension flf. Their content consists of a header and a list of the individual characters. The header essentially contains formatting options, a specification of the character set size and character dimensions. It can also contain free comments from the author. Optionally, the file contents can be compressed in ZIP format.

A key aspect of FIGlet fonts are the formatting rules, which are defined by specifying numerical values in the file header. They determine how the individual characters behave when placed next to or on top of each other. In addition to simple options such as "Full size" (all characters are placed next to each other without modification) and "Fitting" (unnecessary spaces between characters are removed), these include, in particular, the so-called "smushing rules." Using these rules, you can specify, for example, that adjacent characters / and \ can be merged into a single |.
