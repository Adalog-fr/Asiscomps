----------------------------------------------------------------------
--  A4G_Bugs - Package specification                                --
--  Copyright (C) 2005 Adalog                                       --
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

-- This package contains replacements for ASIS operations that are either
-- buggy or not implemented in ASIS-for-GNAT.
-- These operations have the same specification as their "official" counterparts,
-- so that user's code does not need to change when the problems are fixed.
--
-- A comment tells in which version we first observed the problem, and in which version
-- we found it fixed. You may have to experiment if you have an intermediate version.
--
-- When a bug has been fixed, we leave the entry as comments (with an indication of AdaControl's
-- version where the entry has been removed). This may prove useful in case of regression.
-- The entry is removed from the body however; should it need to be resurected, we can get it
-- from VCS
--
-- The replacements are written in such a way that they still work correctly if the
-- bug is fixed; i.e. there is no harm in still using the replacements after the bug
-- has been fixed.

with
  Asis;
package A4G_Bugs is

   --
   -- From Asis.Compilation_Units
   --

   -- Reason      : Unimplemented (at least in compile-on-the-fly mode)
   -- Gnat version: 3.15p
   -- Fixed in    : GPL 2012 (and maybe long before)
   -- Removed from: Adactl 1.14b8
   -- function Subunits (Parent_Body : in Asis.Compilation_Unit) return Asis.Compilation_Unit_List;

   -- Reason      : Returns A_Public_Body instead of A_Public_Declaration_And_Body for spec-less SP.
   -- Bug report  : [EA24-001]
   -- Gnat version: GAP 2005, GnatPro 5.02
   -- Fixed in    : GnatPro 5.04a1, GnatGPL 2006
   -- Removed from: AdaCtl 1.14b8
   -- function Unit_Class (Compilation_Unit : in Asis.Compilation_Unit) return Asis.Unit_Classes;

   --
   -- From Asis.Declarations
   --

   -- Reason      : Endless loop when subtype of T'Base
   -- Bug report  : [EA18-001]
   -- Gnat version: GAP 2005, GnatPro 5.02
   -- Fixed in    : GnatPro 6.1.0
   -- Removed from: AdaCtl 1.14b8
   -- function Corresponding_Last_Subtype (Declaration : in Asis.Declaration) return Asis.Declaration;

   -- Reason      : Renaming of attribute returns A_Function_Call (or A_Procedure_Call_Statement)
   --               instead of An_Attribute_Reference
   -- Bug report  : [FA26-004], 26/10/2006
   -- Gnat version: GNATPro 5.05w (20060603-34)
   -- Fixed in    : GPL 2012 (and likely long before)
   -- Removed from: AdaCtl 1.14b8
   -- function Renamed_Entity (Declaration : in Asis.Declaration) return Asis.Expression;


   --
   -- From Asis.Definitions
   --

   -- Reason      : Infinite loop when T'Base is part of the derivation chain
   -- Bug report  : [F919-016]
   -- Gnat version: GnatPro 5.05w (20060910-34)
   -- Fixed in    : GnatPro 5.05w (200609-)
   -- Removed from: AdaCtl 1.14b8
   -- function Corresponding_Root_Type (Type_Definition : in Asis.Type_Definition) return Asis.Declaration;


   --
   -- From Asis.Elements
   --

   -- Reason      : Incorrect attribute returned
   -- Gnat version: 3.15p
   -- Fixed in    : GnatPro 5.02, GnatGPL 2006
   -- Removed from: AdaCtl 1.14b8
   -- function Attribute_Kind (Expression : in Asis.Expression) return Asis.Attribute_Kinds;


   --
   -- From Asis.Expressions
   --

   -- Reason: Wrong result when Expression is part of implicit
   -- Bug report  : [E225-002]
   -- Gnat version: GAP 1.1.0
   -- Fixed in    : GnatPro 5.02
   -- Removed from: AdaCtl 1.12b1
   -- function Attribute_Designator_Identifier (Expression : in Asis.Expression) return Asis.Expression;

   -- Reason      : Bug when Argument is a call to a child function
   -- Bug report  : [E317-007]
   -- Gnat version: GAP 1.1.0
   -- Fixed in    :
   -- Removed from: Adactl 1.11b6
   -- function Corresponding_Called_Function (Expression : in Asis.Expression) return Asis.Declaration;

   -- Reason      : Bug when Argument is A_Selected_Component or An_Indexed_Component
   --             : Bug when Argument is the selector of A_Selected_Component (sometimes)
   --             : Bug when Argument is An_Explicit_Dereference
   -- Bug report  : [E217-012] [E317-009]
   -- Gnat version: GAP 1.1.0
   -- Fixed in    :
   function Corresponding_Expression_Type (Expression : in Asis.Expression) return Asis.Declaration;

   -- Reason      : In some complicated case involving instantiations of child generic, returns
   --             : A_Defining_Expanded_Name instead of a declaration
   -- Bug report  : [HB03-014]
   -- Gnat version: GnatPro 6.1.2, GnatGPL2008
   -- Fixed in    : GPL2012 (and likely long before)
   -- Removed from: AdaCtl 1.14b8
   -- function Corresponding_Name_Declaration (Reference : in Asis.Expression) return Asis.Element;

   -- Reason      : ASIS failure in some cases (notably with generic formals)
   -- Bug report  : [G223-008]
   -- Gnat version: 6.0.x
   -- Fixed in    : 6.1.?
   -- Removed from: AdaCtl 1.14b8
   -- function Name_Image (Expression : Asis.Expression) return Asis.Program_Text;

   --
   -- From Asis.Statements
   --

   -- Reason      : Bug when Argument is a call to a child function
   -- Bug report  : [E317-007]
   -- Gnat version: GAP 1.1.0
   -- Fixed in    : GnatPro 5.02
   -- Removed from: AdaCtl 1.14b8
   -- function Corresponding_Called_Entity (Statement : in Asis.Statement) return Asis.Declaration;

   -- Reason      : Bug when task body is separate
   -- Bug report  : [H415-017]
   -- Gnat version: GnatPro 6.1.1
   -- Fixed in    : GPL2012 (and likely long before)
   -- Removed from: AdaCtl 1.14b8
   -- function Corresponding_Entry (Statement : in Asis.Statement) return Asis.Declaration;

   ------------------------------------------------------------------------------------------------
   -- Trace identified bugs (in debug mode):

   procedure Trace_Bug (Message : Wide_String);
end A4G_Bugs;
