----------------------------------------------------------------------
--  Utilities - Package specification                               --
--  Copyright (C) 2002 Adalog                                       --
--  Author: J-P. Rosen                                              --
--                                                                  --
--  ADALOG   is   providing   training,   consultancy,   expertise, --
--  assistance and custom developments  in Ada and related software --
--  engineering techniques.  For more info about our services:      --
--  ADALOG                   Tel: +33 1 41 24 31 40                 --
--  19-21 rue du 8 mai 1945  Fax: +33 1 41 24 07 36                 --
--  94110 ARCUEIL            E-m: info@adalog.fr                    --
--  FRANCE                   URL: http://www.adalog.fr              --
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


   --
   --  String facilities
   --

   type Casing is (Lower_Case, Upper_Case, Title_Case);

   function Set_Casing (Item : in Wide_String; To : in Casing) return Wide_String;

   function To_Lower (Item : in Wide_String) return Wide_String;
   --  The language provides this only for String, this is the same for Wide_String
   pragma Inline (To_Lower);

   function To_Upper (Item : in Wide_String) return Wide_String;
   --  The language provides this only for String, this is the same for Wide_String
   pragma Inline (To_Upper);

   function To_Title (Item : in Wide_String) return Wide_String;
   --  Similar to To_Upper, but makes the string Title_Case
   pragma Inline (To_Title);

   function Trim_All (Item : in Wide_String) return Wide_String;
   -- Remove spaces and control characters from both ends of the string
   -- Remove multiple spaces, line delimiters and comments within the string
   -- Lower bound of returned string is 1.

   function Starts_With (Name : Wide_String; Pattern : Wide_String) return Boolean;
   -- Returns True iff the beginning of Name is equal to Pattern

   function Integer_Img (Item : in Integer) return Wide_String;
   -- Like Integer'Wide_Image, without the !*#!! initial space.
   -- (avoids depending on the Gnat specific attribute 'Img)

   function Choose (Preferred : in Wide_String;
                    Otherwise : in Wide_String) return Wide_String;
   --  Returns Preferred if it is not "", Otherwise otherwise

   function Choose (Condition  : in Boolean;
                    When_True  : in Wide_String;
                    When_False : in Wide_String) return Wide_String;
   --  Returns When_True if Condition is True, When_False otherwise

   --
   --  Error
   --

   User_Error : exception;
   procedure Error (Message : Wide_String);
   pragma No_Return (Error);

   procedure Failure (Message : Wide_String);
   procedure Failure (Message : in Wide_String; Element : Asis.Element);
   pragma No_Return (Failure);
   -- Failure raises Program_Error

   --
   -- Debugging facilities
   --

   procedure Set_Trace (File_Name : Wide_String);

   procedure Raw_Trace (Message : Wide_String);

   procedure Trace (Message : Wide_String);
   procedure Trace (Message : Wide_String; Value : Boolean);
   procedure Trace (Message : Wide_String; Value : Integer);
   procedure Trace (Message : Wide_String; Value : Ada.Exceptions.Exception_Occurrence);

   procedure Trace (Message     : Wide_String;
                    Element     : Asis.Element;
                    With_Source : Boolean      := False);
   procedure Trace (Message     : Wide_String;
                    Element     : Asis.Element_List;
                    With_Source : Boolean           := False);

   procedure Assert (Condition : Boolean; Message : Wide_String);
   procedure Assert (Condition : Boolean; Message : Wide_String; Element : Asis.Element);

   procedure Asis_Exception_Messages;

   procedure Traceback (Exc : Ada.Exceptions.Exception_Occurrence);

   --
   --  Safe open
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
end Utilities;
