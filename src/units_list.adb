----------------------------------------------------------------------
--  Units_List - Package body                                       --
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

with   -- Standard Ada units
  Ada.Characters.Handling,
  Ada.Exceptions,
  Ada.IO_Exceptions,
  Ada.Strings.Wide_Maps,
  Ada.Strings.Wide_Fixed,
  Ada.Unchecked_Deallocation,
  Ada.Wide_Characters.Handling;

with   -- ASIS components
  Asis.Clauses,
  Asis.Compilation_Units,
  Asis.Elements,
  Asis.Expressions,
  Asis.Text;

with   -- Reusable components
  Generic_File_Iterator,
  Linear_Queue,
  Thick_Queries,
  Utilities;
package body Units_List is

   ----------------------------------------------------------------
   --                 Internal elements                          --
   ----------------------------------------------------------------

   package String_List is new Linear_Queue (Wide_String);

   ------------------
   -- Global types --
   ------------------

   type Node;
   type Link is access Node;
   type Node (Length : Positive) is
      record
         Value : Wide_String (1..Length);
         Next  : Link;
         Order : Unit_Order;
      end record;
   -- Order defines a position order in the list. There can be gaps in order
   -- between two successive nodes if nodes are deleted.

   type Context_Access is access all Asis.Context;

   ----------------------
   -- Global variables --
   ----------------------

   My_Context : Context_Access;

   Head        : Link;
   Cursor      : Link;
   Previous    : Link;
   List_Length : Natural := 0;
   Max_Order   : Unit_Order := 0;

   ---------
   -- Add --
   ---------

   -- Adds unit to the list, unless it is already there.
   -- Otherwise, add to the tail.
   -- It is OK to add new units while iterating.
   procedure Add (Unit : Wide_String) is
      use Ada.Wide_Characters.Handling;

      Upper_Unit : constant Wide_String := To_Upper (Unit);
      Current    : Link := Head;
   begin
      if Current = null then
         Head        := new Node'(Length => Upper_Unit'Length,
                                  Value  => Upper_Unit,
                                  Next   => null,
                                  Order  => 1);
         Cursor      := Head;
         Previous    := null;
         List_Length := 1;
         Max_Order   := 1;
         return;
      end if;

      loop
         if Current.Value = Upper_Unit then
            return;
         end if;
         exit when Current.Next = null;
         Current := Current.Next;
      end loop;

      -- Not found
      -- Current still points to the last node
      Current.Next := new Node'(Length => Upper_Unit'Length,
                                Value  => Upper_Unit,
                                Next   => null,
                                Order  => Max_Order + 1);
      List_Length := List_Length + 1;
      Max_Order   := Current.Next.Order;
      if Cursor = null then
         Cursor   := Current.Next;
         Previous := Current;
      end if;
   end Add;

   --------------
   -- Trim_All --
   --------------

   Delim_Image : constant Wide_String := Asis.Text.Delimiter_Image;
   function Trim_All (Item : in Wide_String) return Wide_String is
      -- This is a copy of the function in Utilities, to avoid a dependancy on utilities
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


   ----------------------------------------------------------------
   --                 Exported subprograms                       --
   ----------------------------------------------------------------

   ----------
   -- "<=" --
   ----------

   function "<=" (Left, Right : Unit_Order) return Boolean is
   begin
      return Natural (Left) <= Natural (Right);
   end "<=";

   -------------------
   -- Current_Order --
   -------------------

   function Current_Order return Unit_Order is
   begin
      return Cursor.Order;
   end Current_Order;

   ------------------
   -- Current_Unit --
   ------------------

   function Current_Unit return Wide_String is
   begin
      return Cursor.Value;
   end Current_Unit;

   --------------------
   -- Delete_Current --
   --------------------

   procedure Delete_Current is
      procedure Free is new Ada.Unchecked_Deallocation (Node, Link);
      To_Free : Link := Cursor;
   begin
      if Previous = null then
         Head := Cursor.Next;
      else
         Previous.Next := Cursor.Next;
      end if;
      Cursor      := Cursor.Next;
      List_Length := List_Length - 1;
      Free (To_Free);
   end Delete_Current;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Context : access Asis.Context) is
   begin
      My_Context := Context.all'Access;
   end Initialize;

   ------------------
   -- Is_Exhausted --
   ------------------

   function Is_Exhausted return Boolean is
   begin
      return Cursor = null;
   end Is_Exhausted;

   ----------------
   -- Last_Order --
   ----------------

   function Last_Order return Unit_Order is
   begin
      return Max_Order;
   end Last_Order;

   ------------
   -- Length --
   ------------

   function Length return Natural is
   begin
      return List_Length;
   end Length;

   --------------
   -- Register --
   --------------

   procedure Register (Units_Spec : in Wide_String;
                       Recursion  : in Recursion_Mode;
                       Add_Stubs  : in Boolean)
   is
      use Asis, Asis.Compilation_Units;
      use Ada.Strings, Ada.Strings.Wide_Maps;
      use String_List;

      Ignored_Units : String_List.Queue;
      Separators    : constant Wide_Character_Set := To_Set ("+-");

      procedure Raise_Specification_Error (Mess : String);
      pragma No_Return (Raise_Specification_Error);
      procedure Raise_Specification_Error (Mess : String) is
         use Ada.Exceptions;
      begin
         Raise_Exception (Specification_Error'Identity, Mess);
      end Raise_Specification_Error;

      function Must_Ignore (Name : Wide_String) return Boolean is
         -- Check if unit name is either ignored, or a child (or a subunit) of an ignored unit
        C : String_List.Cursor := First (Ignored_Units);
      begin
         while Has_Element (C) loop
            declare
               Unit : constant Wide_String := Fetch (C);
            begin
               if Name = Unit
                 or else (Name'Length > Unit'Length + 1
                   and then Unit = Name (Name'First .. Name'First+Unit'Length-1)
                          and then Name (Name'First + Unit'Length) = '.')
               then
                  return True;
               end if;
            end;
            C := Next (C);
         end loop;
         return False;
      end Must_Ignore;

      procedure Process_Dependents (Unit_Spec, Unit_Body : Compilation_Unit) is

         procedure Process_Unit (My_Unit : Compilation_Unit) is
            use Asis.Clauses, Asis.Elements;

            procedure Add_Withed_Unit (Withed_Name : Asis.Expression; Is_Limited : Boolean) is
               use Ada.Strings.Wide_Fixed;
               use Asis.Expressions, Asis.Text;
               use Thick_Queries;
               Unit_Name : Asis.Expression;
            begin
               if Expression_Kind (Withed_Name) = A_Selected_Component then
                  -- Must add all units in the prefix
                  Add_Withed_Unit (Prefix (Withed_Name), Is_Limited);

                  -- Treat this one
                  Unit_Name := Selector (Withed_Name);
               else
                  Unit_Name := Withed_Name;
               end if;

               if Is_Limited then
                  -- Horrible kludge to work around ASIS bugs with limited with
                  -- Get the name of the package from the text, to get the (full) unit
                  -- This assumes that the name is "reasonable" (not partial due to use clauses, not spanned over
                  -- several lines...)
                  declare
                     Full_Name: constant Wide_String := Trim (Element_Image (Withed_Name), Both);
                  begin
                     --## rule off use_ultimate_origin ## We have no element to call Ultimate_Origin with
                     if Unit_Origin (Library_Unit_Declaration (Full_Name, My_Context.all)) = An_Application_Unit then
                        Add (Full_Name);
                     end if;
                     --## rule on use_ultimate_origin
                  end;
               else
                  declare
                     Name_Def : constant Asis.Definition := Corresponding_Name_Definition (Unit_Name);
                  begin
                     if Unit_Origin (Enclosing_Compilation_Unit         --## rule line off Use_Ultimate_Origin
                                     (Name_Def)) = An_Application_Unit  --   we want to keep library unit renamings
                     then
                        Add (Full_Name_Image (Name_Def));
                     end if;
                  end;
               end if;
            end Add_Withed_Unit;

         begin   -- Process_Unit
            for CC : Asis.Element of Context_Clause_Elements (My_Unit) loop
               if Clause_Kind (CC) = A_With_Clause then
                  for Unit : Asis.Name of Clause_Names (CC) loop
                     Add_Withed_Unit (Unit, Trait_Kind (CC) = A_Limited_Trait);
                  end loop;
               end if;
            end loop;
         end Process_Unit;

         Good_Spec : Compilation_Unit;
         Good_Body : Compilation_Unit;
      begin   -- Process_Dependents
         case Recursion is
            when None =>
               return;
            when Spec_Only  =>
               Good_Spec := Unit_Spec;
               Good_Body := Nil_Compilation_Unit;
            when Spec_Closure =>
               if Is_Nil (Unit_Spec)
                 and then not Is_Nil (Unit_Body)
                 and then Unit_Kind (Unit_Body) in A_Procedure_Body .. A_Function_Body
               then
                  -- Subprogram without explicit specification => body acts as declaration, process its clauses
                  -- Subunits are purposedly not included, since the body of a subunit is not part of the required
                  -- closure for compilation.
                  Good_Spec := Nil_Compilation_Unit;
                  Good_Body := Unit_Body;
               else
                  Good_Spec := Unit_Spec;
                  -- It is the caller's responsibility to filter out bodies that are not required
                  Good_Body := Unit_Body;
               end if;
            when Full =>
               Good_Spec := Unit_Spec;
               Good_Body := Unit_Body;
         end case;

         if not Is_Nil (Good_Spec) then
            Process_Unit (Unit_Spec);
         end if;

         if not Is_Nil (Good_Body) then
            Process_Unit (Unit_Body);
         end if;
      end Process_Dependents;

      procedure Do_Process_Stub (My_Unit : Compilation_Unit) is
         use Ada.Wide_Characters.Handling;
      begin
         -- NB: A protected body can be a compilation unit if it is a subunit,
         --     but it cannot contain stubs.
         if Is_Nil (My_Unit) or else Unit_Kind (My_Unit) = A_Protected_Body_Subunit then
            return;
         end if;

         if Unit_Origin (My_Unit) /= An_Application_Unit or else   --## rule line off Use_Ultimate_Origin
           Must_Ignore (To_Upper (Unit_Full_Name (My_Unit)))       --   it's a unit here, and renamings have no stubs
         then
            return;
         end if;

         for CU : Asis.Compilation_Unit of Asis.Compilation_Units.Subunits (My_Unit) loop
            -- We do not add stubs if Add_Stubs is false, but if recursive, we still need
            -- to add units that are withed by the stub
            if Add_Stubs then
               Add (Unit_Full_Name (CU));
            end if;

            if Recursion /= None then
               Process_Dependents (Unit_Spec => Nil_Compilation_Unit, Unit_Body => CU);
            end if;
         end loop;
      end Do_Process_Stub;

      procedure Process_Units_Spec (Spec : Wide_String) is
         use Ada.Characters.Handling, Ada.Wide_Characters.Handling, Ada.Strings.Wide_Fixed;

         procedure Process_Indirect_File (Name : String) is
            use Ada.Exceptions, Ada.IO_Exceptions;

            procedure Action (Line : String) is
               Good_Line : constant Wide_String := Trim_All (To_Wide_String (Line));
               Stop      : constant Natural:= Index (Good_Line, " ");
            begin
               if Good_Line /= ""
                 and then Good_Line (1) /= '#'
                 and then (Good_Line'Length = 1 or else Good_Line (1 .. 2) /= "--")
               then
                  if Stop = 0 then
                     Process_Units_Spec (Good_Line);
                  else
                     Process_Units_Spec (Good_Line (Good_Line'First .. Stop - 1));
                  end if;
               end if;
            end Action;

            procedure Iterate is new Generic_File_Iterator (Action);
         begin  -- Process_Indirect_File
            Iterate (Name);
         exception
            when Name_Error =>
               Raise_Specification_Error ("Missing units file: " & Name);
            when Occur : others =>
               Raise_Specification_Error ("Exception while processing " & Name & ": " & Exception_Name (Occur));
         end Process_Indirect_File;

         procedure Add_With_Parent (Unit : Wide_String) is
            -- Pre-condition: Unit is all upper-case
            Inx   : Natural;
         begin
            -- Add parent if any before the unit
            Inx := Index (Unit, ".", Going => Backward);
            if Inx /= 0 then
               Add_With_Parent (Unit (Unit'First .. Inx - 1));
            end if;
            Add (Unit);
         end Add_With_Parent;

         Start : Positive;
         Stop  : Natural;
      begin  -- Process_Units_Spec
         --
         -- Get rid of case of indirect file:
         --
         if Spec (Spec'First) = '@' then
            if Spec'Length = 1 then
               -- '@' alone
               Raise_Specification_Error ("Missing file name after @");
            end if;

            Process_Indirect_File (To_String (Utilities.Clean_File_Name (Spec (Spec'First + 1 .. Spec'Last))));
            return;
         end if;

         --
         -- Extract unit names and ignored units from unit spec.
         --

         if Spec (Spec'First) = '+' or Spec (Spec'First) = '-' then
            Start := Spec'First + 1;
         else
            Start := Spec'First;
         end if;
         loop
            Stop := Index (Spec (Start .. Spec'Last), Separators);
            if Stop = 0 then
               Stop := Spec'Last;
            elsif Stop = Start then   -- "--" or "+-" or "-+" or "++" ...
               Raise_Specification_Error ("No unit name after '-' or '+'");
            else
               Stop := Stop - 1;
            end if;
            if Start = Spec'First or else Spec (Start-1) = '+' then
               Add_With_Parent (To_Upper (Spec (Start .. Stop)));
            else
               Append (Ignored_Units, To_Upper (Spec (Start .. Stop)));
            end if;
            exit when Stop = Spec'Last;
            Start := Stop + 2;
         end loop;
      end Process_Units_Spec;

      Unit_Found      : Boolean := False;
      Specified_Limit : Unit_Order;  -- Order of last node from Unit_Spec
                                     -- Following nodes are added from stubs/with clauses
   begin  -- Register
      -- Build list of units

      Process_Units_Spec (Units_Spec);
      if Is_Exhausted then
         -- No units were added (empty indirect file for example)
         return;
      end if;
      Specified_Limit := Current_Order;

      --  Process list of units
      Reset;
      while not Is_Exhausted loop
         declare
            This_Unit : constant Wide_String := Current_Unit;
            Spec_Decl : Asis.Compilation_Unit;
            Body_Decl : Asis.Compilation_Unit;
         begin
            if Must_Ignore (This_Unit) then
               Delete_Current;
            else
               -- Temporary fix for problem with System
               -- Make sure at least one unit is open before accessing anything else
               if not Unit_Found or Add_Stubs or Recursion /= None then
                  Body_Decl  := Compilation_Unit_Body (This_Unit, My_Context.all);
                  Unit_Found := not Is_Nil (Body_Decl);
               end if;
               if not Unit_Found or Recursion /= None then
                  Spec_Decl  := Library_Unit_Declaration (This_Unit, My_Context.all);
                  Unit_Found := not Is_Nil (Spec_Decl);
               end if;

               -- Search for subunits stubs
               -- Stubs can only appear in bodies
               -- If recursive, stubs must be parsed since they can have their own
               -- "with" clauses
               if Add_Stubs or Recursion /= None then
                  Do_Process_Stub (Body_Decl);
               end if;

               -- Analyze with clauses
               if Recursion = Full or Current_Order <= Specified_Limit then
                  Process_Dependents (Spec_Decl, Body_Decl);
               else
                  Process_Dependents (Spec_Decl, Nil_Compilation_Unit);
               end if;
               Skip;
            end if;
         end;
      end loop;
   end Register;

   -----------
   -- Reset --
   -----------

   procedure Reset is
   begin
      Cursor   := Head;
      Previous := null;
   end Reset;

   ----------
   -- Skip --
   ----------

   procedure Skip is
   begin
      Previous := Cursor;
      Cursor   := Cursor.Next;
   end Skip;

end Units_List;
