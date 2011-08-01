----------------------------------------------------------------------
--  A4G_Bugs - Package specification                                --
--  Copyright (C) 2005 Adalog                                       --
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

-- This package contains replacements for ASIS operations that are either
-- buggy or not implemented in ASIS-for-GNAT.
-- These operations have the same specification as their "official" counterparts,
-- so that user's code does not need to change when the problems are fixed.
--
-- A comment tells in which version we observed the problem, and in which version
-- we found it fixed. You may have to experiment if you have an intermediate version.
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
   -- Fixed in    : ?
   function Subunits (Parent_Body : in Asis.Compilation_Unit) return Asis.Compilation_Unit_List;



   --
   -- From Asis.Elements
   --

   -- Reason      : Incorrect attribute returned
   -- Gnat version: 3.15p
   -- Fixed in    : ?
   function Attribute_Kind (Expression : in Asis.Expression) return Asis.Attribute_Kinds;



   --
   -- From Asis.Declarations
   --

   -- Reason      : Does not (not always?) follow a chain of renamings
   -- Bug report  : [E901-003]
   -- Gnat version: GAP 2005
   -- Fixed in    : current wavefront, not released
   function Corresponding_Base_Entity (Declaration : in Asis.Declaration) return Asis.Expression;



   --
   -- From Asis.Expressions
   --

   -- Reason      : Bug when Argument is A_Selected_Component or An_Indexed_Component
   --             : Bug when Argument is the selector of A_Selected_Component (sometimes)
   --             : Bug when Argument is An_Explicit_Dereference
   -- Bug report  : [E217-012] [E317-009]
   -- Gnat version: GAP 1.1.0
   -- Fixed in    : current wavefront, not released
   function Corresponding_Expression_Type (Expression : in Asis.Expression) return Asis.Declaration;

   -- Reason: Wrong result when Expression is part of implicit
   -- Bug report  : [E225-002]
   -- Gnat version: GAP 1.1.0
   function Attribute_Designator_Identifier (Expression : in Asis.Expression) return Asis.Expression;

   -- Reason      : Bug when Argument is a call to a child function
   -- Bug report  : [E317-007]
   -- Gnat version: GAP 1.1.0
   -- Fixed in    : current wavefront, not released
   function Corresponding_Called_Function (Expression : in Asis.Expression) return Asis.Declaration;


   --
   -- From Asis.Statements
   --

   -- Reason      : Bug when Argument is a call to a child procedure
   -- Bug report  : [E317-007]
   -- Gnat version: GAP 1.1.0
   -- Fixed in    : current wavefront, not released
   function Corresponding_Called_Entity (Statement : in Asis.Statement) return Asis.Declaration;
end A4G_Bugs;
