----------------------------------------------------------------------
--  Producer - Package body                                         --
--  Copyright (C) 2002-2017 Adalog                                  --
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

with   -- with for KLUDGE, to be removed when A4G fixed
  Asis.Compilation_Units,
  Asis.Elements,   -- For KLUDGE
  Ada.Strings.Wide_Fixed;

with   -- Standard Ada units
  Ada.Characters.Handling,
  Ada.Unchecked_Deallocation,
  Ada.Wide_Text_IO;

with   -- Application specific units
  Utilities;
package body Producer is

   -- Positions from source
   Current_State : State;

   -- Position in output
   Current_Line : Line_Number := 1;

   -- Local buffer and its management variables
   -- Dynamically allocated since it depends on the -l option
   type Wide_String_Access is access Wide_String;
   Buffer     : Wide_String_Access;
   Buffer_Inx : Natural := 0;
   Cut_Point  : Natural := 0;
   In_Quotes  : Boolean := False;
   In_Comment : Boolean := False;

   -- Global state of the producer
   Print_Changed_Lines   : Boolean := False;
   There_Is_Substitution : Boolean := False;
   Insert_Count          : Line_Number := 0;
   Global_Changes        : Boolean := False;
   Col_At_Insert         : Character_Position;
   Push_Count            : Natural := 0;

   ---------------------------------------------------------------------------------
   -----------------------------  Internal utilities  ------------------------------
   ---------------------------------------------------------------------------------

   -- Forward declarations:

   -- The following subprograms do the equivalent of their Wide_Text_IO counterparts,
   -- but manage local buffering to allow splitting of long lines.
   -- To avoid confusion, we always use explicit qualification when calling the
   -- Wide_Text_IO operations. (Next_Line is not called New_Line since it is slightly
   -- different).
   -- ONLY these subprograms call Wide_Text_IO.
   procedure Put       (Item : Wide_Character);
   procedure Put       (Item : Wide_String);
   procedure Set_Col   (To : in Character_Position);
   function  Col       return Character_Position;

   ---------
   -- Col --
   ---------

   function Col return Character_Position is
   begin
      if Buffer = null then
         return Character_Position (Ada.Wide_Text_IO.Col);
      else
         return Character_Position (Buffer_Inx + 1);
      end if;
   end Col;

   ------------------
   -- Is_Delimiter --
   ------------------

   Delimiter : constant array (Character) of Boolean :=  -- ARM 2.2 (3..11)
     (Character'First .. ' ' |
      '&' | ''' | '(' | ')' | '*' | '+' | ',' | '-' |
      '.' | '/' | ':' | ';' | '<' | '=' | '>' | '|' => True,
      others => False);

   function  Is_Delimiter (C : Wide_Character) return Boolean is
      use Ada.Characters.Handling;
   begin
      return Delimiter (To_Character (C, ' '));
   end Is_Delimiter;

   --------------------
   -- Finish_Inserts --
   --------------------

   procedure Finish_Inserts is
      -- If lines were inserted, provide the --CHANGED line if necessary.
   begin
      if Push_Count > 0 then
         -- Everything is considered as inserted lines when inside the
         -- rewind span, or not at basic push level. Do nothing until we're outside.
         return;
      end if;

      if Insert_Count /= 0 then
         Next_Line;
         Global_Changes := True;
         if Print_Changed_Lines then
            Put ("--CHANGED:");
            Put (Line_Number'Wide_Image (Insert_Count));
            Put (" line(s) inserted");
            Next_Line;
         end if;
         Set_Col (Col_At_Insert);
         Insert_Count := 0;
      end if;
   end Finish_Inserts;

   --------------------------
   -- New_Line_And_Comment --
   --------------------------

   procedure New_Line_And_Comment (Old_Line : Asis.Text.Line; Conditional : Boolean := False) is
      -- If Conditional = True, do not make a New_Line if the current (output) line is empty
   begin
      Next_Line (Conditional);
      if There_Is_Substitution then
         Global_Changes := True;
         if Print_Changed_Lines then
            Put ("--CHANGED:");
            Put (Line_Image (Old_Line));
            Next_Line;
         end if;
         There_Is_Substitution := False;
      end if;
   end New_Line_And_Comment;

   ---------------
   -- Next_Line --
   ---------------

   procedure Next_Line (Conditional : Boolean := False)
   is
   begin
      if not Conditional or else Col /= 1 then
         if Buffer = null then
            Ada.Wide_Text_IO.New_Line;
         else
            Ada.Wide_Text_IO.Put_Line (Buffer (1..Buffer_Inx));
            Buffer_Inx := 0;
            Cut_Point  := 0;
            In_Comment := False;
            In_Quotes  := False;
         end if;
         Current_Line := Current_Line + 1;
         if Push_Count > 0 then   -- Count inserts
            Insert_Count := Insert_Count + 1;
         end if;
      end if;
   end Next_Line;

   --------------------
   -- Print_Comments --
   --------------------

   procedure Print_Comments (Image : Wide_String) is
      type Parsing_State is (String, Comment, Outside);
      Current_Parsing_State : Parsing_State := Outside;
   begin
      for I in Image'Range loop
         case Current_Parsing_State is
            when String =>
               if Image (I) = '"' then
                  Current_Parsing_State := Outside;
               end if;
            when Comment =>
               Put (Image (I));
            when Outside =>
               case Image (I) is
                  when '"' =>
                     Current_Parsing_State := String;
                  when '-' =>
                     if I /= Image'Last and then Image (I+1) = '-' then
                        Put ('-');
                        Current_Parsing_State := Comment;
                     end if;
                  when others =>
                     null;
               end case;
         end case;
      end loop;
   end Print_Comments;

   ---------
   -- Put --
   ---------

   procedure Put (Item : Wide_Character) is
      use Utilities;
   begin
      if Buffer = null then
         Ada.Wide_Text_IO.Put (Item);
         return;
      end if;

      if Buffer_Inx = Buffer'Last then
         -- Reached max line length
         if Cut_Point /= 0 then
            -- Output up to cut point and retry with the rest
            declare
               Extra_Part : constant Wide_String := Buffer (Cut_Point + 1 .. Buffer'Last) & Item;
            begin
               Buffer_Inx := Cut_Point;
               Next_Line;
               Put (Extra_Part);
               return;
            end;
         end if;

         -- No cut point here
         if In_Comment then
            -- Split comment on two lines
            Next_Line;
            Put ("-- ");
         elsif In_Quotes then
            -- Concatenate on two lines
            declare
               Save_Last : constant Wide_Character := Buffer (Buffer'Last);
            begin
               Buffer (Buffer'Last) := '"';
               In_Quotes := False;
               Next_Line;
               Put ("& """);
               Put (Save_Last);
               -- Should be OK now, proceed normally
            end;
         else
            -- Nothing we can do, generate something that does not compile
            Next_Line;
            User_Message ("Unable to cut long line, output line:" & Line_Number'Wide_Image (Output_Line));
            Put ("?Line cut? ");
         end if;
      end if;

      -- Here, everything normal; add character to buffer
      Buffer_Inx := Buffer_Inx + 1;
      Buffer (Buffer_Inx) := Item;

      -- For the moment, we allow to cut after exposed spaces and special characters
      -- that do not require complicated checks for a compound delimiter, except that we
      -- have to care about comments. We do not allow ' either (because of ''').
      -- (OK, we are a bit lazy here :-)
      if not In_Comment then
         case Item is
            when ' ' | '+' | ',' | ';' | '&' | '(' | ')' | '|' =>
               -- We can always break after these characters
               if not In_Quotes then
                  Cut_Point := Buffer_Inx;
               end if;
            when '/' | ':' =>
               -- We can always break before these characters
               if not In_Quotes then
                  Cut_Point := Buffer_Inx - 1;
               end if;
            when '"' =>
               -- Since we do not allow to cut just before or after character strings,
               -- it will properly handle the case of "This is a quote""".
               In_Quotes := not In_Quotes;
            when '-' =>
               if not In_Quotes and then Buffer_Inx > 1 and then Buffer (Buffer_Inx - 1) = '-' then
                  -- Comment
                  Cut_Point  := Buffer_Inx - 2;
                  In_Comment := True;
               end if;
            when others =>
               null;
         end case;
      end if;
   end Put;

   ---------
   -- Put --
   ---------

   procedure Put (Item : Wide_String) is
   begin
      for C : Wide_Character of Item loop
         Put (C);
      end loop;
   end Put;

   -------------
   -- Set_Col --
   -------------

   procedure Set_Col (To : in Character_Position) is
   begin
      if To < Col then
         -- Unlike Text_IO, we do not allow this
         raise Program_Error;
      end if;
      if Buffer = null then
         Ada.Wide_Text_IO.Set_Col (Ada.Wide_Text_IO.Positive_Count(To));
      else
         Buffer (Buffer_Inx + 1 .. Natural (To) - 1) := (others => ' ');
         Buffer_Inx := Natural (To) - 1;
      end if;
   end Set_Col;

   ---------------------------------------------------------------------------------
   -----------------------------  Exported operations  -----------------------------
   ---------------------------------------------------------------------------------

   -------------
   -- Advance --
   -------------

   procedure Advance (The_Element : Element; Included : Boolean := True) is
      -- Note that every line in the span of the skipped element must be
      -- considered as "changed".
      The_Span            : constant Span := Element_Span (The_Element);
      Advance_Last_Line   : Line_Number;
      Advance_Last_Column : Character_Position;
   begin
      -- Get rid of annoying special case
      if Is_Nil (The_Span) then
         return;
      end if;

      if Included then
         Advance_Last_Line   := The_Span.Last_Line;
         Advance_Last_Column := The_Span.Last_Column;
      else
         Advance_Last_Line   := The_Span.First_Line;
         Advance_Last_Column := The_Span.First_Column - 1;
      end if;

      Finish_Inserts;

      declare
         The_Lines : constant Line_List := Lines (The_Element,
                                                  First_Line => Current_State.Last_Printed_Line,
                                                  Last_Line  => Advance_Last_Line);
      begin
         There_Is_Substitution := True;
         if The_Lines'Length = 1 then
            -- Everything fits on same line as previous element
            -- The following call to Print_Comment should not be necessary, as
            -- comments are necessarily after the span of the element.
            -- However, it is safer in the case of strange spans, and it is harmless to
            -- call it anyway
            Print_Comments (Line_Image (The_Lines (The_Lines'First))
                            (Current_State.Last_Printed_Column+1 .. Advance_Last_Column));
         else
            Print_Comments (Line_Image (The_Lines (The_Lines'First))
                            (Current_State.Last_Printed_Column+1 .. Length (The_Lines (The_Lines'First))));
            New_Line_And_Comment (The_Lines (The_Lines'First));

            for L : Line of The_Lines (The_Lines'First+1 .. The_Lines'Last-1) loop
               There_Is_Substitution := True;
               Print_Comments (Line_Image (L));
               New_Line_And_Comment (L, Conditional => True);
            end loop;

            There_Is_Substitution := True;
            Print_Comments (Line_Image (The_Lines (The_Lines'Last))
                          (1 .. Advance_Last_Column));
         end if;
      end;

      Current_State := (Last_Printed_Line   => Advance_Last_Line,
                        Last_Printed_Column => Advance_Last_Column);
   end Advance;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Set_Line_Length : Natural; Set_Print_Changed_Lines : Boolean) is
      procedure Free is new Ada.Unchecked_Deallocation (Wide_String, Wide_String_Access);
   begin
      Print_Changed_Lines := Set_Print_Changed_Lines;

      if Buffer /= null then
         if Buffer'Length = Set_Line_Length then
            -- No need to reallocate;
            return;
         end if;
         Free (Buffer);
      end if;

      if Set_Line_Length /= 0 then
         Buffer := new Wide_String (1..Set_Line_Length);
      end if;
   end Initialize;

   -----------------
   -- Output_Line --
   -----------------

   function Output_Line return Line_Number is
   begin
      return Current_Line;
   end Output_Line;

   -----------
   -- Print --
   -----------

   procedure Print (Item : Wide_String) is
   begin
      -- Print of a null string has no effect:
      if Item = "" then
         return;
      end if;

      Finish_Inserts;
      Put (Item);
      There_Is_Substitution := True;
   end Print;

   ----------------
   -- Print_Line --
   ----------------

   procedure Print_Line (Item : Wide_String; In_Col : Character_Position := 0) is
   begin
      if Insert_Count = 0 then
         -- First insert
         Col_At_Insert := Col;
      end if;

      if Current_State.Last_Printed_Line /= 1 or Current_State.Last_Printed_Column /= 0 then
         -- Do not call Next_Line if nothing has been printed yet
         Next_Line;
      end if;

      if In_Col /= 0 then
         Set_Col (In_Col);
      end if;
      Put (Item);

      Insert_Count := Insert_Count + 1;
   end Print_Line;

   ---------------------------
   -- Print_Up_To (Element) --
   ---------------------------

   procedure Print_Up_To (The_Element : Element;
                          Included    : Boolean;
                          Changing    : Wide_String := "";
                          Into        : Wide_String := "";
                          Final       : Boolean     := False)
   is
      use Utilities;

      The_Span    : constant Span := Element_Span (The_Element);
      Last_Line   : Line_Number;
      Last_Column : Character_Position;

      procedure Print_Substituted (Item : Wide_String) is
         I : Positive;
      begin
         -- First, get rid of trivial cases
         if Item = "" then
            return;
         end if;

         if Changing = "" or else Changing = Into then
            Put (Item);
            return;
         end if;

         -- Now, do the real stuff
         I := Item'First;
         loop
            if I > Item'Last - Changing'Length + 1                                      -- Not enough remaining space
               or else (I <= Item'Last-1 and then (Item (I) = '-' and Item(I+1) = '-')) -- Comment
            then
               -- Don't try to substitute further
               Put (Item (I..Item'Last));
               exit;
            end if;

            -- Do the substitution if Changing matches the upcomming string,
            -- making sure it is a full identifier
            -- (characters before and after are separators)
            if Set_Casing (Changing, Upper_Case) = Set_Casing (Item (I .. I + Changing'Length - 1), Upper_Case)
              and then (I = Item'First or else Is_Delimiter (Item (I - 1)))
              and then (I+Changing'Length-1 = Item'Last or else Is_Delimiter (Item (I+Changing'Length)))
            then
               Put (Into);
               I := I + Changing'Length;
               There_Is_Substitution := True;
            else
               Put (Item (I));
               I := I + 1;
            end if;
         end loop;
      end Print_Substituted;

   begin  -- Print_Up_To

      -- Get rid of annoying special case
      -- (Can happen with implicit declarations, for example)
      if Is_Nil (The_Span) then
         return;
      end if;

      if Included then
         Last_Line   := The_Span.Last_Line;
         Last_Column := The_Span.Last_Column;
      else
         Last_Line   := The_Span.First_Line;
         Last_Column := The_Span.First_Column - 1;
      end if;

      if Final then
         Assert (Included, "Final not included");
         -- We must include possible comment lines after the end of the unit
         Last_Line   := Compilation_Span (The_Element).Last_Line;
         Last_Column := Compilation_Span (The_Element).Last_Column;
         if Last_Column = 0 then
            -- In this case, A4G sometimes return 0 instead of the Last of the last line
            Last_Column := Line_Image (Lines (The_Element, Last_Line, Last_Line)(Last_Line))'Last;
         end if;
      end if;

      -- KLUDGE for bug in A4G
      -- If we have something like "V-1" (without space between "V" and "-"), the span
      -- for Identifier V and Parameter_Association for V includes the "-".
      -- This bug happens only for "-", if there is no space between "V" and "-".
      -- Fix the span if an Identifier includes "-".
      -- We cannot do the same for a Parameter_Association, since it can include any expression.
      -- Currently, the translator makes sure not to call Print_Up_To with a Parameter_Association.
      if Asis.Elements.Element_Kind (The_Element) = An_Expression and then
        Asis.Elements.Expression_Kind (The_Element) = An_Identifier and then
        Ada.Strings.Wide_Fixed.Index (Element_Image (The_Element), "-") /= 0
      then
         Last_Column :=  Ada.Strings.Wide_Fixed.Index (Element_Image (The_Element), "-") - 1;
      end if;

      -- Check that we do not move backward (null move is OK)
      -- This is a very important check, as most of client's bugs are trapped here!
      Assert (Last_Line > Current_State.Last_Printed_Line or else (Last_Line = Current_State.Last_Printed_Line and
                                                                   Last_Column >= Current_State.Last_Printed_Column),
              "Illegal span in Print_Up_To, "
              & Asis.Compilation_Units.Text_Name (Asis.Elements.Enclosing_Compilation_Unit (The_Element)) & ": ("
              & Line_Number'Wide_Image (Current_State.Last_Printed_Line) & ','
              & Character_Position'Wide_Image (Current_State.Last_Printed_Column) & ") ("
              & Line_Number'Wide_Image (Last_Line) & ','
              & Character_Position'Wide_Image (Last_Column) & ')',
              The_Element
             );

      if Last_Line   = Current_State.Last_Printed_Line   and
         Last_Column = Current_State.Last_Printed_Column and  -- Everything already printed
         not Final    -- If final = True, the new_line (and commented original line)
      then            -- must still be printed
         return;
      end if;

      Finish_Inserts;

      declare
         The_Lines : constant Line_List := Lines (The_Element,
                                                  First_Line => Current_State.Last_Printed_Line,
                                                  Last_Line  => Last_Line);
      begin
         if The_Lines'Length = 1 then
            -- Everything fits on same line as previous element
            Print_Substituted (Line_Image (The_Lines (The_Lines'First))
                               (Current_State.Last_Printed_Column+1 .. Last_Column));
         else
            Print_Substituted (Line_Image (The_Lines (The_Lines'First))
                               (Current_State.Last_Printed_Column+1 .. Length (The_Lines (The_Lines'First))));
            New_Line_And_Comment (The_Lines (The_Lines'First));

            for L : Line of The_Lines (The_Lines'First+1 .. The_Lines'Last-1) loop
               Print_Substituted (Line_Image (L));
               New_Line_And_Comment (L);
           end loop;

            -- Last_Column seems unreliable in some cases.
            -- Since it should never be outside the line range, taking the 'min prevents
            -- some stupid Constraint_Error...
            Print_Substituted (Line_Image (The_Lines (The_Lines'Last))
                               (1 .. Character_Position'Min (Last_Column,
                                                             Line_Image (The_Lines (The_Lines'Last))'Last)
                               )
                              );
         end if;
         if Final then
            -- Case of the final line of a unit
            -- Make sure we have a new line, and that the last CHANGED: is
            -- issued if necessary
            New_Line_And_Comment (The_Lines (The_Lines'Last));
         end if;
      end;

      Current_State := (Last_Printed_Line   => Last_Line,
                        Last_Printed_Column => Last_Column);
   end Print_Up_To;

   --------------------------------
   -- Print_Up_To (Element_List) --
   --------------------------------

   procedure Print_Up_To (The_List    : Element_List;
                          Included    : Boolean;
                          Changing    : Wide_String := "";
                          Into        : Wide_String := "";
                          Final       : Boolean     := False)
   is
      use Asis.Elements;
   begin
      if Is_Nil (The_List) then
         return;
      end if;

      if Included then
         Print_Up_To (The_List (The_List'Last), Included, Changing, Into, Final);
      else
         Print_Up_To (The_List (The_List'First), Included, Changing, Into, Final);
      end if;
   end Print_Up_To;

   --------------------------------
   -- Print_Up_To  (Wide_String) --
   --------------------------------

   procedure Print_Up_To (The_Word : Wide_String; Included : Boolean; Ref_Elem : Element) is
      use Utilities;
   begin
      loop
         declare
            The_Line : constant Program_Text := Line_Image (Lines
                                                            (Ref_Elem,
                                                             First_Line => Current_State.Last_Printed_Line,
                                                             Last_Line  => Current_State.Last_Printed_Line)
                                                            (Current_State.Last_Printed_Line));
         begin
            for I in Positive range Current_State.Last_Printed_Column + 1 .. The_Line'Last loop
               -- We use a "for" loop here, but we always exit through the "exit" below:
               if I > The_Line'Last - The_Word'Length + 1    -- Not enough remaining space for The_Word
                 or else (I <= The_Line'Last - 1 and then (The_Line (I) = '-' and The_Line (I + 1) = '-')) -- Comment
               then
                  exit;

               elsif Set_Casing (The_Line (I .. I + The_Word'Length - 1), Upper_Case) = The_Word -- The_Word found
                 and then (I = The_Line'First
                           or else Is_Delimiter (The_Line (I - 1)))
                 and then (I + The_Word'Length - 1 = The_Line'Last
                           or else Is_Delimiter (The_Line (I + The_Word'Length)))
               then
                  if Included then
                     Put (The_Line (I .. I + The_Word'Length - 1));
                     Current_State.Last_Printed_Column := I + The_Word'Length - 1;
                  end if;
                  return;
               else
                  Put (The_Line (I));
                  Current_State.Last_Printed_Column := I;
               end if;
            end loop;
            Put (The_Line (Current_State.Last_Printed_Column + 1 .. The_Line'Last));
            Next_Line;
            Current_State := (Last_Printed_Line => Current_State.Last_Printed_Line + 1, Last_Printed_Column => 0);
         end;
      end loop;
   end Print_Up_To;

   ------------
   -- Finish --
   ------------

   procedure Finish (Had_Changes : out Boolean) is
   begin
      Had_Changes := Global_Changes;

      -- Reset variables for next unit
      Current_State         := (Last_Printed_Line   => 1,
                                Last_Printed_Column => 0);
      There_Is_Substitution := False;
      Global_Changes        := False;
   end Finish;

   -----------------
   -- Push_Source --
   -----------------

   procedure Push_Source (Into : out State; From_Line : Line_Number := 1; From_Col : Character_Position := 0) is
   begin
      Push_Count    := Push_Count + 1;
      Into          := Current_State;
      Current_State := (Last_Printed_Line   => From_Line,
                        Last_Printed_Column => From_Col-1);
   end Push_Source;

   ----------------
   -- Pop_Source --
   ----------------

   procedure Pop_Source  (From : in State) is
      use Utilities;
   begin
      Assert (Push_Count > 0, "Unmatched push count");
      Push_Count    := Push_Count - 1;
      Current_State := From;
   end Pop_Source;

end Producer;
