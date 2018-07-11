----------------------------------------------------------------------
--  Utilities - Package specification                               --
--  Copyright (C) 2002 Adalog                                       --
--  Author: J-P. Rosen                                              --
--                                                                  --
--  ADALOG   is   providing   training,   consultancy,   expertise, --
--  assistance and custom developments  in Ada and related software --
--  engineering techniques.  For more info about our services:      --
--  ADALOG                          Tel: +33 1 45 29 21 52          --
--  2 rue du Docteur Lombard        Fax: +33 1 45 29 25 00          --
--  92441 ISSY LES MOULINEAUX CEDEX E-m: info@adalog.fr             --
--  FRANCE                          URL: http://www.adalog.fr       --
--                                                                  --
--  This  unit is  free software;  you can  redistribute  it and/or --
--  modify  it under  terms of  the GNU  General Public  License as --
--  published by the Free Software Foundation; either version 2, or --
--  (at your  option) any later version.  This  unit is distributed --
--  in the hope  that it will be useful,  but WITHOUT ANY WARRANTY; --
--  without even the implied warranty of MERCHANTABILITY or FITNESS --
--  FOR A  PARTICULAR PURPOSE.  See the GNU  General Public License --
--  for more details.   You should have received a  copy of the GNU --
--  General Public License distributed  with this program; see file --
--  COPYING.   If not, write  to the  Free Software  Foundation, 59 --
--  Temple Place - Suite 330, Boston, MA 02111-1307, USA.           --
--                                                                  --
--  As  a special  exception, if  other files  instantiate generics --
--  from  this unit,  or you  link this  unit with  other  files to --
--  produce an executable,  this unit does not by  itself cause the --
--  resulting executable  to be covered  by the GNU  General Public --
--  License.  This exception does  not however invalidate any other --
--  reasons why  the executable  file might be  covered by  the GNU --
--  Public License.                                                 --
----------------------------------------------------------------------

with  -- Standard Ada units
  Ada.Exceptions,
  Ada.Wide_Text_IO;

with  -- Asis
  Asis;
package Utilities is
   pragma Elaborate_Body (Utilities);

   --
   --  Output control options
   --

   Verbose_Option   : Boolean := False;
   Debug_Option     : Boolean := False;

   Error_Is_Out     : Boolean := False;
   -- The variable above tells whether Current_Error and Current_Output are the same
   -- (i.e. Current_Output has not been redirected), to improve the presentation of
   -- User_Message. It is initialized to false so that by default, if not properly
   -- initialized, the output will be less pretty, but not wrong.

   Trace_Is_Error   : Boolean := True;
   -- Tells whether trace is to current_error, or has been redirected to a file

   --
   --  Output
   --

   procedure User_Message (Message : Wide_String := ""; Stay_On_Line : Boolean := False);
   procedure User_Log     (Message : Wide_String := ""; Stay_On_Line : Boolean := False);
   -- Output only if Verbose_Option

   function Format_Duration (How_Long : Duration) return Wide_String;

   --
   --  String facilities
   --

   type Casing is (Lower_Case, Upper_Case, Title_Case);

   function Set_Casing (Item : in Wide_String; To : in Casing) return Wide_String;

   function To_Lower (Item : in Wide_String) return Wide_String;
   --  The language provides this only for String, this is the same for Wide_String (fixed in 2012)
   pragma Inline (To_Lower);

   function To_Upper (Item : in Wide_String) return Wide_String;
   --  The language provides this only for String, this is the same for Wide_String (fixed in 2012)
   pragma Inline (To_Upper);

   function To_Title (Item : in Wide_String) return Wide_String;
   --  Similar to To_Upper, but makes the string Title_Case
   pragma Inline (To_Title);

   function Quote (Item : in Wide_String) return Wide_String;
   -- Surrounds Item with double quotes; any inner double quote is doubled

   function Trim_All (Item : in Wide_String) return Wide_String;
   -- Remove spaces and control characters from both ends of the string
   -- Remove multiple spaces, line delimiters and comments within the string
   -- Lower bound of returned string is 1.

   function Starts_With (Name : Wide_String; Pattern : Wide_String) return Boolean;
   -- Returns True iff the beginning of Name is equal to Pattern

   function Integer_Img (Item : in Integer) return Wide_String;
   -- Like Integer'Wide_Image, without the !*#!! initial space.

   function ASIS_Integer_Img (Item : in Asis.ASIS_Integer) return Wide_String;
   -- Same thing for ASIS_Integer (which is the type of Line_Number and Column_Position)
   -- Depending on ASIS implementation, this may be the same as Integer (ASIS-for-Gnat) or not (Gela)

   function Choose (Preferred : in Wide_String;
                    Otherwise : in Wide_String) return Wide_String;
   --  Returns Preferred if it is not "", Otherwise otherwise

   function Choose (Condition  : in Boolean;
                    When_True  : in Wide_String;
                    When_False : in Wide_String) return Wide_String;
   --  Returns When_True if Condition is True, When_False otherwise

   function Adjust_Image (Original : Wide_String) return Wide_String;
   -- Transform a Full_Name_Image according to the syntax we use externally.
   -- The differences with the string return by Full_Name_Image are:
   --   we use "return" rather than ":" for the return type of functions.
   --   we use "access" rather than "*" for access parameters


   --
   --  Error
   --

   User_Error : exception;
   procedure Error (Message : Wide_String);
   pragma No_Return (Error);
   -- Error raises User_Error;

   procedure Failure (Message : Wide_String);
   procedure Failure (Message : in Wide_String; Element : Asis.Element);
   procedure Failure (Message : in Wide_String; Occur   : Ada.Exceptions.Exception_Occurrence);
   pragma No_Return (Failure);
   -- Failure raises Program_Error

   procedure Unimplemented (Origin : Wide_String; Fatal : Boolean := True);
   -- Unimplemented calls Failure if Fatal, Trace otherwise


   --
   -- Debugging facilities
   --

   procedure Set_Trace (File_Name : Wide_String);

   procedure Raw_Trace (Message : Wide_String);

   procedure Trace (Message : Wide_String);
   function  Trace (Message : Wide_String) return Boolean;   -- Always returns True
   procedure Trace (Message : Wide_String; Value : Wide_String);
   procedure Trace (Message : Wide_String; Value : Boolean);
   procedure Trace (Message : Wide_String; Value : Integer);
   procedure Trace (Message : Wide_String; Value : Ada.Exceptions.Exception_Occurrence);

   procedure Trace (Message     : Wide_String;
                    Element     : Asis.Element;
                    With_Source : Boolean      := True);
   function  Trace (Message     : Wide_String;
                    Element     : Asis.Element;
                    With_Source : Boolean      := True) return Boolean;   -- Always returns True

   procedure Trace (Message      : Wide_String;
                    Element_List : Asis.Element_List;
                    With_Source  : Boolean      := True);
   function  Trace (Message      : Wide_String;
                    Element_List : Asis.Element_List;
                    With_Source  : Boolean      := True) return Boolean;   -- Always returns True

   procedure Assert (Condition : Boolean; Message : Wide_String);
   procedure Assert (Condition : Boolean; Message : Wide_String; Element : Asis.Element);

   procedure Asis_Exception_Messages;

   procedure Stack_Traceback (Exc : Ada.Exceptions.Exception_Occurrence);

   --
   --  File facilities
   --
   Overwrite_Error : exception;

   type Open_Mode is (Create, Append);
   -- Create: the file must not exist, unless Overwrite_Option
   -- Append: if the file exists, it is appended to, unless Overwrite_Option
   -- Failure of the existence condition raises Overwrite_Error
   procedure Safe_Open (File : in out Ada.Wide_Text_IO.File_Type;
                        Name : String;
                        Mode : Open_Mode;
                        Overwrite_Option : Boolean);

   function Locate_Regular_File (File_Name : Wide_String; Path_Variable : Wide_String) return Wide_String;
   -- Like Gnat.OS_Lib.Locate_Regular_File, except that:
   -- - Parameters and result are Wide_String
   -- - The name of an OS variable is given
   -- - It returns a Wide_String, no risk of memory leak
   -- (and of course, this hides the dependency to Gnat.OS_Lib in the body).

   function Clean_File_Name (File_Name : Wide_String) return Wide_String;
   -- Cleans up File_Name from artifacts due to the (dubious!) processing of parameters
   -- by the environment:
   -- - if File_Name is surrounded by '"', remove them
   -- - replace all "\ " by " "

end Utilities;
