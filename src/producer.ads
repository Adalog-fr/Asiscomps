----------------------------------------------------------------------
--  Producer - Package specification                                --
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

with -- Asis components
  Asis.Text;
package Producer is
   use Asis, Asis.Text;

   -- Initialize module and set line length
   -- It is allowed to call it multiple times
   procedure Initialize (Set_Line_Length : Natural; Set_Print_Changed_Lines : Boolean);

   -- Terminate current unit
   procedure Finish (Had_Changes : out Boolean);

   -- Print some text at the current position
   procedure Print (Item : Wide_String);

   -- Start a new (output) line. If conditional = True, do nothing if already in column 1
   procedure Next_Line (Conditional : Boolean := False);

   -- Inserts a full line in the output.
   -- Does not change the current point.
   -- Text starts at column In_Col (current column if = 0)
   procedure Print_Line (Item : Wide_String; In_Col : Character_Position := 0);

   -- Print program text up to just before (Included=False) or just
   -- after (Included=True) the given element.
   -- If Changing /= "", then every appearance of Changing outside a comment
   -- is replaced by Into (almost dumb substitution).
   -- Final = True forces a new_line (and the printing of the original line if -c)
   --    Useful only for the final Print_Up_To that ensures that the unit is
   --    fully printed.
   procedure Print_Up_To (The_Element : Element;
                          Included    : Boolean;
                          Changing    : Wide_String := "";
                          Into        : Wide_String := "";
                          Final       : Boolean     := False);

   -- Similarly for Element_List
   -- If included = True,  same as Print_Up_To (The_List (The_List'Last), ...
   -- If included = False, same as Print_Up_To (The_List (The_List'First), ...
   -- Does nothing if The_List = Nil_Element_List
   procedure Print_Up_To (The_List    : Element_List;
                          Included    : Boolean;
                          Changing    : Wide_String := "";
                          Into        : Wide_String := "";
                          Final       : Boolean     := False);

   -- Advances current point until just before (Included=False) or just after(Included=True)
   -- the given element.
   -- Ada elements between the current source position and the begining (respectively end) of
   -- the element are discarded, but comments and line breaks are printed.
   procedure Advance (The_Element : Element; Included : Boolean := True);

   -- Returns current point to the start (if Included) or the end (if not Included)
   -- of the given element.
   -- Everything generated from this point until the current point will be considered
   -- an insertion (since it has already been produced once).
   -- Multiple Rewind are allowed.
   procedure Rewind (The_Element : Element; Included : Boolean := False);

   -- Where are we in the output file ?
   function Output_Line return Line_Number;

end Producer;
