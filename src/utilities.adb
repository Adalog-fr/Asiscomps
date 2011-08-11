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
  Ada.Strings.Wide_Fixed,
  Ada.Strings.Wide_Unbounded,
  Ada.Strings.Wide_Maps.Wide_Constants;

with  -- ASIS units
  Asis.Compilation_Units,
  Asis.Elements,
  Asis.Errors,
  Asis.Implementation,
  Asis.Text;
package body Utilities is

   -- Note that we delay opening the actual trace file until traces are
   -- actually used, to avoid creating the trace file if not used.
   Trace_Name    : Ada.Strings.Wide_Unbounded.Unbounded_Wide_String;
   Trace_File    : aliased Ada.Wide_Text_IO.File_Type;
   Current_Trace : Ada.Wide_Text_IO.File_Access := Ada.Wide_Text_IO.Current_Error;

   ----------------
   -- Trace_Elem --
   ----------------

   procedure Trace_Elem (Element : Asis.Element) is
      use Ada.Wide_Text_IO;
      use Asis, Asis.Compilation_Units, Asis.Elements, Asis.Text;
      S : constant Span := Element_Span (Element);
   begin
      Put (Current_Trace.all, Element_Kinds'Wide_Image (Element_Kind (Element)));
      if not Is_Nil (Element) then
         Put (Current_Trace.all, " => ");
         case Element_Kind (Element) is
            when Not_An_Element =>
               -- Impossible actually, but we don't feel like calling Failure from here
               null;
            when A_Pragma =>
               Put (Current_Trace.all, Pragma_Kinds'Wide_Image (Pragma_Kind (Element)));
            when A_Defining_Name =>
               Put (Current_Trace.all, Defining_Name_Kinds'Wide_Image (Defining_Name_Kind (Element)));
            when A_Declaration =>
               Put (Current_Trace.all, Declaration_Kinds'Wide_Image (Declaration_Kind (Element)));
            when A_Definition =>
               Put (Current_Trace.all, Definition_Kinds'Wide_Image (Definition_Kind (Element)));
            when An_Expression =>
               Put (Current_Trace.all, Expression_Kinds'Wide_Image (Expression_Kind (Element)));
            when An_Association =>
               Put (Current_Trace.all, Association_Kinds'Wide_Image (Association_Kind (Element)));
            when A_Statement =>
               Put (Current_Trace.all, Statement_Kinds'Wide_Image (Statement_Kind (Element)));
            when A_Path =>
               Put (Current_Trace.all, Path_Kinds'Wide_Image (Path_Kind (Element)));
            when A_Clause =>
               Put (Current_Trace.all, Clause_Kinds'Wide_Image (Clause_Kind (Element)));
            when An_Exception_Handler =>
               null;
            when others =>
               -- Corresponds to GNAT extensions: An_Expression_Path
               Put (Current_Trace.all, "(non-standard)");
         end case;

         Put (Current_Trace.all, " at ");
         Put (Current_Trace.all, Text_Name (Enclosing_Compilation_Unit (Element)));
         Put (Current_Trace.all, ':');
         Put (Current_Trace.all, Integer_Img (S.First_Line));
         Put (Current_Trace.all, ':');
         Put (Current_Trace.all, Integer_Img (S.First_Column));
      end if;
      New_Line (Current_Trace.all);
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
   -- Assert --
   ------------

   procedure Assert (Condition : Boolean; Message : Wide_String; Element : Asis.Element) is
   begin
      if not Condition then
         Failure (Message, Element);
      end if;
   end Assert;

   ------------
   -- Choose --
   ------------

   function Choose (Preferred : in Wide_String;
                    Otherwise : in Wide_String) return Wide_String is
   begin
      if Preferred = "" then
         return Otherwise;
      else
         return Preferred;
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

   begin  -- Failure
      Trace ("Failing element " & Span_Image (Element_Span (Element)), Element); --## rule line off no_trace
      Failure (Message);
   end Failure;

   -----------------
   -- Starts_With --
   -----------------

   function Starts_With (Name : Wide_String; Pattern : Wide_String) return Boolean is
   begin
      return Name'Length >= Pattern'Length
        and then Name (Name'First .. Name'First + Pattern'Length - 1) = Pattern;
   end Starts_With;

   --------------
   -- User_Log --
   --------------

   procedure User_Log (Message : Wide_String; Stay_On_Line : Boolean := False) is
   begin
      if Verbose_Option then
        User_Message (Message, Stay_On_Line);
      end if;
   end User_Log;

   ------------------
   -- User_Message --
   ------------------

   procedure User_Message (Message : Wide_String; Stay_On_Line : Boolean := False) is
      use Ada.Wide_Text_IO;
      Old_Col : Count;
   begin

      if Error_Is_Out then
         Old_Col := Col (Current_Output);
         Set_Col (Current_Output, 1);
      end if;

      Put (Current_Error, Message);
      if not Stay_On_Line then
         New_Line (Current_Error);
      end if;

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

   ----------------
   -- Set_Casing --
   ----------------

   function Set_Casing (Item : in Wide_String; To : in Casing) return Wide_String is
   begin
      case To is
         when Upper_Case =>
            return To_Upper (Item);
         when Lower_Case =>
            return To_Lower (Item);
         when Title_Case =>
            return To_Title (Item);
      end case;    -- Really end case!
   end Set_Casing;

   --------------
   -- To_Lower --
   --------------

   function To_Lower (Item : in Wide_String) return Wide_String is
      use Ada.Strings.Wide_Fixed, Ada.Strings.Wide_Maps.Wide_Constants;
   begin
      return Translate (Item, Lower_Case_Map);
   end To_Lower;

   --------------
   -- To_Upper --
   --------------

   function To_Upper (Item : in Wide_String) return Wide_String is
      use Ada.Strings.Wide_Fixed, Ada.Strings.Wide_Maps.Wide_Constants;
   begin
      return Translate (Item, Upper_Case_Map);
   end To_Upper;

   --------------
   -- To_Title --
   --------------

   function To_Title (Item : in Wide_String) return Wide_String is
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

   --------------
   -- Trim_All --
   --------------

   Delim_Image : constant Wide_String := Asis.Text.Delimiter_Image;
   function Trim_All (Item : in Wide_String) return Wide_String is
      Result    : Wide_String (1 .. Item'Length);
      Last      : Natural  := 0;
      Start     : Positive := Item'First;
      Stop      : Natural  := Item'Last;
      In_Quotes : Boolean := False;
      In_Comment: Boolean := False;
      Delim_Inx : Natural := 0;
   begin
      for I in Item'Range loop
         if Item (I) > ' ' then
            Start := I;
            Last  := 1;
            Result (1) := Item (I);
            exit;
         end if;
      end loop;
      if Last = 0 then
         -- Nothing found
         return "";
      end if;

      for I in reverse Item'Range loop
         if Item (I) > ' ' then
            Stop := I;
            if Stop = Start then
               -- Only one character
               return Result (1 .. 1);
            end if;
            exit;
         end if;
      end loop;

      -- Since we loop until Stop-1, it is safe to access Item (I+1)
      for I in Positive range Start+1 .. Stop-1 loop
         if In_Quotes then
            Last          := Last + 1;
            Result (Last) := Item (I);
         elsif In_Comment then
            if Delim_Inx = 0 then
               if Item (I) = Delim_Image (Delim_Image'First) then
                  Delim_Inx := Delim_Image'First;
               end if;
            elsif Item (I) /= Delim_Image (Delim_Inx) then
               Delim_Inx := 0;
            elsif Delim_Inx = Delim_Image'Last then
               In_Comment := False;
            end if;
         else
            case Item (I) is
               when Wide_Character'First .. Wide_Character'Pred (' ') =>
                  null;
               when '"' =>
                  In_Quotes     := not In_Quotes;
                  Last          := Last + 1;
                  Result (Last) := '"';
               when '-' =>
                  if Item (I+1) = '-' then
                     In_Comment := True;
                  else
                     Last          := Last + 1;
                     Result (Last) := Item (I);
                  end if;
               when ' ' =>
                  if Item (I+1) /= ' ' then
                     Last          := Last + 1;
                     Result (Last) := ' ';
                  end if;
               when others =>
                  Last          := Last + 1;
                  Result (Last) := Item (I);
            end case;
         end if;
      end loop;
      Last          := Last + 1;
      Result (Last) := Item (Stop);

      return Result (1 .. Last);
   end Trim_All;

   -----------------
   -- Integer_Img --
   -----------------

   function Integer_Img (Item : in Integer) return Wide_String is
      Result : constant Wide_String := Integer'Wide_Image (Item); --## Rule line OFF Use_Img_Function
      subtype Slide is Wide_String (1 .. Result'Length-1);
   begin
      if Item < 0 then
         return Result;
      else
         return Slide (Result (2 .. Result'Last));
      end if;
   end Integer_Img;

   ---------------
   -- Set_Trace --
   ---------------

   procedure Set_Trace (File_Name : Wide_String) is
      use Ada.Strings.Wide_Unbounded, Ada.Wide_Text_IO;
   begin
      if Is_Open (Trace_File) then
         Close (Trace_File);
      end if;

      if File_Name = "" or else To_Upper (File_Name) = "CONSOLE" then
         Current_Trace  := Current_Error;
         Trace_Is_Error := True;
      else
         Current_Trace  := Trace_File'Access;
         Trace_Name     := To_Unbounded_Wide_String (File_Name);
         Trace_Is_Error := False;
      end if;
   end Set_Trace;

   ---------------
   -- Raw_Trace --
   ---------------

   procedure Raw_Trace (Message : Wide_String) is
      use Ada.Characters.Handling, Ada.Strings.Wide_Unbounded, Ada.Wide_Text_IO;
   begin
      if not Is_Open (Current_Trace.all) then
         Safe_Open (Trace_File, To_String (To_Wide_String (Trace_Name)), Append, Overwrite_Option => False);
      end if;
      Put_Line (Current_Trace.all, Message);
   end Raw_Trace;

   --## Rule off no_trace ## Trace SP can use each other

   ------------
   -- Trace  --
   ------------

   procedure Trace (Message : Wide_String) is
   begin
      if Debug_Option then
         Raw_Trace ("<<" & Message &  ">>");
      end if;
   end Trace;

   ------------
   -- Trace  --
   ------------

   procedure Trace (Message : Wide_String; Value : Boolean) is
   begin
      Trace (Message & ", value= " & Boolean'Wide_Image (Value));
   end Trace;

   ------------
   -- Trace  --
   ------------

   procedure Trace (Message : Wide_String; Value : Integer) is
   begin
      Trace (Message & ", value= " & Integer_Img (Value));
   end Trace;

   ------------
   -- Trace  --
   ------------

   procedure Trace (Message : Wide_String; Value : Ada.Exceptions.Exception_Occurrence) is
      use Ada.Characters.Handling, Ada.Exceptions;
   begin
      Trace (Message & ", exception info= " & To_Wide_String (Exception_Information (Value)));
   end Trace;

   ------------
   -- Trace  --
   ------------

   procedure Trace (Message     : Wide_String;
                    Element     : Asis.Element;
                    With_Source : Boolean      := False) is
      use Asis.Text;
   begin
      if Debug_Option then
         Raw_Trace ( "<<" & Message);

         Trace_Elem (Element);

         if With_Source and not Is_Nil (Element) Then
            Raw_Trace (Element_Image (Element));
         end if;

         Raw_Trace(">>");
      end if;
   end Trace;

   ------------
   -- Trace  --
   ------------

   procedure Trace (Message     : Wide_String;
                    Element     : Asis.Element_List;
                    With_Source : Boolean           := False) is
      use Asis.Text;
   begin
      if Debug_Option then
         Raw_Trace("<<" & Message);

         for E in Element'Range loop
            Trace_Elem (Element (E));

            if With_Source then
               Raw_Trace (Element_Image (Element (E)));
            end if;
         end loop;

         Raw_Trace (">>");
      end if;
   end Trace;

   --## rule on no_trace

end Utilities;

