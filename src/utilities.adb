----------------------------------------------------------------------
--  Utilities - Package body                                        --
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

with   -- Standard Ada units
  Ada.Characters.Handling,
  Ada.Wide_Text_IO,
  Ada.Strings.Wide_Maps.Wide_Constants;

with  -- Standard Ada units
  Ada.Exceptions,
  Ada.Characters.Handling;
with  -- ASIS units
  Asis.Elements,
  Asis.Errors,
  Asis.Implementation,
  Asis.Text;
package body Utilities is

   ----------------
   -- Trace_Elem --
   ----------------

   procedure Trace_Elem (Element : Asis.Element) is
      use Ada.Wide_Text_IO, Asis, Asis.Elements;
   begin
      Put (Current_Error, Element_Kinds'Wide_Image (Element_Kind (Element)));
      Put (Current_Error, " => ");
      case Element_Kind (Element) is
         when Not_An_Element =>
            null;
         when A_Pragma =>
            Put (Current_Error, Pragma_Kinds'Wide_Image (Pragma_Kind (Element)));
         when A_Defining_Name =>
            Put (Current_Error, Defining_Name_Kinds'Wide_Image (Defining_Name_Kind (Element)));
         when A_Declaration =>
            Put (Current_Error, Declaration_Kinds'Wide_Image (Declaration_Kind (Element)));
         when A_Definition =>
            Put (Current_Error, Definition_Kinds'Wide_Image (Definition_Kind (Element)));
         when An_Expression =>
            Put (Current_Error, Expression_Kinds'Wide_Image (Expression_Kind (Element)));
         when An_Association =>
            Put (Current_Error, Association_Kinds'Wide_Image (Association_Kind (Element)));
         when A_Statement =>
            Put (Current_Error, Statement_Kinds'Wide_Image (Statement_Kind (Element)));
         when A_Path =>
            Put (Current_Error, Path_Kinds'Wide_Image (Path_Kind (Element)));
         when A_Clause =>
            Put (Current_Error, Clause_Kinds'Wide_Image (Clause_Kind (Element)));
         when An_Exception_Handler =>
            null;
      end case;
      New_Line (Current_Error);
   end Trace_Elem;

   -----------------------------
   -- Asis_Exception_Messages --
   -----------------------------

   procedure Asis_Exception_Messages is
      use Asis.Errors, Asis.Implementation;
   begin
      User_Message ("Status   : " & Error_Kinds'Wide_Image (Status));
      User_Message ("Diagnosis: " & Diagnosis);
   end Asis_Exception_Messages;

   ------------
   -- Assert --
   ------------

   procedure Assert (Condition : Boolean; Message : Wide_String) is
   begin
      if not Condition then
         Failure (Message);
      end if;
   end Assert;

   ------------
   -- Choose --
   ------------

   function Choose (Preferred : in Wide_String;
                    Otherwise : in Wide_String) return Wide_String is
   begin
      if Preferred /= "" then
         return Preferred;
      else
         return Otherwise;
      end if;
   end Choose;

   ------------
   -- Choose --
   ------------

   function Choose (Condition  : in Boolean;
                    When_True  : in Wide_String;
                    When_False : in Wide_String) return Wide_String is
   begin
      if Condition then
         return When_True;
      else
         return When_False;
      end if;
   end Choose;

   -----------
   -- Error --
   -----------

   procedure Error (Message : Wide_String) is
      use Ada.Exceptions, Ada.Characters.Handling;
   begin
      Raise_Exception (User_Error'Identity, To_String (Message));
   end Error;

   -------------
   -- Failure --
   -------------

   procedure Failure (Message : Wide_String) is
      use Ada.Exceptions, Ada.Characters.Handling;
   begin
      Raise_Exception (Program_Error'Identity, To_String (Message));
   end Failure;

   -------------
   -- Failure --
   -------------

   procedure Failure (Message : in Wide_String; Element : Asis.Element) is
      use Asis.Text;
      function Span_Image (S : Span) return Wide_String is
      begin
         return
           '('
           & Line_Number_Positive'Wide_Image (S.First_Line)
           & ','
           & Character_Position_Positive'Wide_Image (S.First_Column)
           & "), ("
           & Line_Number_Positive'Wide_Image (S.Last_Line)
           & ','
           & Character_Position_Positive'Wide_Image (S.Last_Column)
           & ')';
      end Span_Image;
   begin
      Trace ("Failing element " & Span_Image (Element_Span (Element)), Element); --## rule line off no_trace
      Failure (Message);
   end Failure;

   --------------
   -- User_Log --
   --------------

   procedure User_Log (Message : Wide_String) is
   begin
      if Verbose_Option then
        User_Message (Message);
      end if;
   end User_Log;

   ------------------
   -- User_Message --
   ------------------

   procedure User_Message (Message : Wide_String) is
      use Ada.Wide_Text_IO;
      Old_Col : Count;
   begin

      if Error_Is_Out then
         Old_Col := Col (Current_Output);
         Set_Col (Current_Output, 1);
      end if;

      Put_Line (Current_Error, Message);

      if Error_Is_Out then
         Set_Col (Current_Output, Old_Col);
      end if;
   end User_Message;

   ---------------
   -- Safe_Open --
   ---------------

   procedure Safe_Open (File : in out Ada.Wide_Text_IO.File_Type;
                        Name : String;
                        Mode : Open_Mode;
                        Overwrite_Option : Boolean)
   is
      use Ada.Wide_Text_IO;
   begin
      if Overwrite_Option then
         Create (File, Out_File, Name);
      else
         begin
            case Mode is
               when Create =>
                  Open (File, Out_File, Name);

                  -- File exists
                  Close (File);
                  raise Overwrite_Error;
               when Append =>
                  Open (File, Append_File, Name);
                  -- OK if file exists
            end case;

         exception
            when Name_Error =>
               -- File does not exist (either mode)
               Create (File, Out_File, Name);
         end;
      end if;
   exception
      when Occur : others =>
         declare
            use Ada.Exceptions;
         begin
            Raise_Exception (Exception_Identity (Occur),
                             Message => "Error opening " & Name &
                                        " for " & Open_Mode'Image (Mode));
         end;
   end Safe_Open;

   --------------
   -- To_Lower --
   --------------

   function To_Lower (Item : in Wide_String) return Wide_String is
      use Ada.Strings.Wide_Maps, Ada.Strings.Wide_Maps.Wide_Constants;

      Result : Wide_String (1 .. Item'Length);
   begin
      for I in Item'Range loop
         Result (I - (Item'First - 1)) := Value (Lower_Case_Map, Item (I));
      end loop;

      return Result;
   end To_Lower;

   --------------
   -- To_Upper --
   --------------

   function To_Upper (Item : in Wide_String) return Wide_String is
      use Ada.Strings.Wide_Maps, Ada.Strings.Wide_Maps.Wide_Constants;

      Result : Wide_String (1 .. Item'Length);
   begin
      for I in Item'Range loop
         Result (I - (Item'First - 1)) := Value (Upper_Case_Map, Item (I));
      end loop;

      return Result;
   end To_Upper;

   --------------
   -- To_Title --
   --------------

   function To_Title (Item : Wide_String) return Wide_String is
      use Ada.Strings.Wide_Maps, Ada.Strings.Wide_Maps.Wide_Constants;

      Result     : Wide_String (1 .. Item'Length);
      Capitalize : Boolean := True;
      --  True if next character should be upper case
   begin
      for I in Item'Range loop
         if Capitalize then
            Result (I - (Item'First - 1)) := Value (Upper_Case_Map, Item (I));
         else
            Result (I - (Item'First - 1)) := Value (Lower_Case_Map, Item (I));
         end if;
         Capitalize := Item (I) not in 'a' .. 'z' and
                       Item (I) not in 'A' .. 'Z' and
                       Item (I) not in '0' .. '9';
      end loop;

      return Result;
   end To_Title;

   ------------
   -- Trace  --
   ------------

   procedure Trace (Message : Wide_String) is
      use Ada.Wide_Text_IO;
   begin
      if Debug_Option then
         Put (Current_Error, "<<");
         Put (Current_Error, Message);
         Put_Line (Current_Error, ">>");
      end if;
   end Trace;

   ------------
   -- Trace  --
   ------------

   procedure Trace (Message     : Wide_String;
                    Element     : Asis.Element;
                    With_Source : Boolean      := False) is
      use Ada.Wide_Text_IO, Asis.Text;
   begin
      if Debug_Option then
         Put (Current_Error, "<<");
         Put (Current_Error, Message);
         New_Line (Current_Error);

         Trace_Elem (Element);

         if With_Source then
            Put_Line (Current_Error, Element_Image (Element));
         end if;

         Put_Line (Current_Error, ">>");
      end if;
   end Trace;

   ------------
   -- Trace  --
   ------------

   procedure Trace (Message     : Wide_String;
                    Element     : Asis.Element_List;
                    With_Source : Boolean           := False) is
      use Ada.Wide_Text_IO, Asis.Text;
   begin
      if Debug_Option then
         Put (Current_Error, "<<");
         Put (Current_Error, Message);
         New_Line (Current_Error);

         for E in Element'Range loop
            Trace_Elem (Element (E));

            if With_Source then
               Put_Line (Current_Error, Element_Image (Element (E)));
            end if;
         end loop;

         Put_Line (Current_Error, ">>");
      end if;
   end Trace;

end Utilities;

