package body Ordered_Maps_G is

   procedure Get (M       : Map;
                  Key     : in  Key_Type;
                  Value   : out Value_Type;
                  Success : out Boolean) is
      
      P_Aux : Cell_Array := M.P_Array;
      I     : Natural;
      
   begin
   
      Success := False;
   
      if M.Length /= 0 then
      
         I := (M.Length / 2) + 1;
         
         if P_Aux(I).Key < Key then
            -- Moverme hacia la derecha --
            loop
            
               exit when I = M.Length + 1;
               
               if P_Aux(I).Key = Key then
                  -- Encontrado --
                  Success := True;
                  Value   := P_Aux(I).Value;
                  exit;
                  
               else
                  -- Pasamos al siguiente --
                  I := I + 1;
               
               end if;
            
            end loop;
            
         elsif Key < P_Aux(I).Key then
            -- Moverme hacia la izquierda --
            loop
            
               exit when I = 0;
            
               if P_Aux(I).Key = Key then
                  -- Encontrado --
                  Success := True;
                  Value   := P_Aux(I).Value;
                  exit;
                  
               else
                  -- Pasamos al anterior --
                  I := I - 1;
               
               end if;
            
            end loop;
         
         else
            -- Encontrado --
            Success := True;
            Value   := P_Aux(I).Value;
         
         end if;
      
      elsif M.Length = Max then
         -- Mapa lleno --
         raise Full_Map;
         
      end if;
      
   end Get;

   procedure Put (M     : in out Map;
                  Key   : Key_Type;
                  Value : Value_Type) is
   
      Found : Boolean    := False;
      P_Aux : Cell_Array := M.P_Array;
      I     : Natural;
      
   begin
      
      if M.Length = Max then
         -- Maximo alcanzado --
         raise Full_Map;
         
      end if;
   
      case (M.Length) is
      
         when 0 =>
            -- Es el primer cliente --
            M.P_Array(1).Key   := Key;
            M.P_Array(1).Value := Value;
            M.Length           := M.Length + 1;
            
         when others =>
            -- Nos colocamos en medio --
            I := (M.Length / 2) + 1;
            
            if P_Aux(I).Key < Key then
               -- Hacia derecha --
               loop
               
                  exit when I = M.Length + 1;
            
                  if P_Aux(I).Key < Key then
                     -- Pasamos al siguiente --
                     I := I + 1;
                  
                  elsif Key < P_Aux(I).Key then
                     -- Nos salimos --
                     exit;
                  
                  else
                     -- Actualizamos --
                     P_Aux(I).Value := Value;
                     Found          := True;
                     exit;
                  
                  end if;
               
               end loop;
               
               if not Found then
                  -- No esta en la lista --
                  if I = M.Length + 1 then
                     -- Añadir el ultimo --
                     P_Aux(M.Length + 1).Key   := Key;
                     P_Aux(M.Length + 1).Value := Value;
                  
                  else
                     -- Añadir en medio --
                     P_Aux(I + 1..M.Length + 1) := P_Aux(I..M.Length);
                     P_Aux(I).Key           := Key;
                     P_Aux(I).Value         := Value;
                  
                  end if;
                  
                  M.P_Array := P_Aux;
                  M.Length  := M.Length + 1;
               
               end if;
            
            elsif Key < P_Aux(I).Key then
               -- Hacia la izquierda --
               loop
               
                  exit when I = 0;
            
                  if Key < P_Aux(I).Key then
                     -- iteramos --
                     I := I - 1;
                  
                  elsif P_Aux(I).Key < Key then
                     -- nos salimos --
                     exit;
                  
                  else
                     -- actualizamos --
                     P_Aux(I).Value := Value;
                     Found          := True;
                     exit;
                  
                  end if;
               
               end loop;
               
               if not Found then
                  -- no esta en la lista --
                  if I = 0 then
                     -- es el primero --
                     P_Aux(2..M.Length + 1) := M.P_Array(1..M.Length);
                     P_Aux(1).Key           := Key;
                     P_Aux(1).Value         := Value;
                  
                  else
                     -- añadir en medio --
                     P_Aux(I + 2..M.Length + 1) := M.P_Array(I + 1..M.Length);
                     P_Aux(I + 1).Key           := Key;
                     P_Aux(I + 1).Value         := Value;
                  
                  end if;
                  
                  M.P_Array := P_Aux;
                  M.Length  := M.Length + 1;
               
               end if;
               
            else
               -- encontrado --
               Found          := True;
               P_Aux(I).Value := Value;
               M.P_Array      := P_Aux;
            
            end if;
      
      end case;
      
   end Put;

   procedure Delete (M       : in out Map;
                     Key     : in  Key_Type;
                     Success : out Boolean) is 
   
      I     : Natural    := 1;
      P_Aux : Cell_Array := M.P_Array;
      Value : Value_Type;
      
   begin
   
      Get(M, Key, Value, Success);
      
      if Success then
      
         if M.Length /= 0 then
      
            I := (M.Length / 2) + 1;
            
            if P_Aux(I).Key < Key then
               -- hacia derecha --
               loop
               
                  exit when I = M.Length + 1;
                  
                  if P_Aux(I).Key = Key then
                     -- encontrado --
                     exit;
                     
                  else
                     -- iteramos --
                     I := I + 1;
                  
                  end if;
               
               end loop;
               
            elsif Key < P_Aux(I).Key then
               -- hacia izquierda --
               loop
               
                  exit when I = 0;
               
                  if P_Aux(I).Key = Key then
                     -- encontrado --
                     exit;
                     
                  else
                     -- iteramos --
                     I := I - 1;
                  
                  end if;
               
               end loop;
            
            end if;
            
            P_Aux(I..M.Length - 1) := P_Aux(I + 1..M.Length);
            M.Length           := M.Length - 1;
            M.P_Array          := P_Aux;
         
         elsif M.Length = Max then
            -- maximo de clientes --
            raise Full_Map;
            
         end if;
       
      end if;
   
   end Delete;

   function Map_Length (M : Map) return Natural is
   
   begin
   
      return M.Length;
      
   end Map_Length;

   function First (M : Map) return Cursor is
   
      C : Cursor;
   
   begin
   
      C.Pos := 1;
      C.M   := M;
   
      return C;
      
   end First;

   procedure Next (C : in out Cursor) is
   
   begin
   
       C.Pos := C.Pos + 1;
       
   end Next;

   function Has_Element (C : Cursor) return Boolean is
   
   begin
   
      return C.Pos <= C.M.Length;
      
   end Has_Element;

   function Element (C: Cursor) return Element_Type is
   
      E : Element_Type;
      
   begin
   
      if C.Pos > C.M.Length then
      
         raise No_Element;
         
      else
      
         E.Key   := C.M.P_Array(C.Pos).Key;
         E.Value := C.M.P_Array(C.Pos).Value;
         
         return E;
         
      end if;

   end Element;

end Ordered_Maps_G;
