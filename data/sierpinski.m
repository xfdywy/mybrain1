function sierpinski(A, B, C, level) 
% SIERPINSKI    Recursively generated Sierpinski triangle. 
%                          sierpinski(PA, PB, PC, LVL) generates an approximation to 
%                          the Sierpinski triangle, where the 2-vectors PA, PB and PC 
%                          define the triangle vertices. 
%                          LVL is the level of recursion. 
if level == 0 
      % Fill the triangle with vertices A, B, C. 
      fill ([A(1), B(1), C(1)], [A(2), B(2), C(2)], [0.0 0.0 0.0]); 
      hold on 
else 
      % Recursive calls for the three subtriangles 
      sierpinski(A, (A + B)/2, (A + C)/2, level-1) 
      sierpinski(B, (B + A)/2, (B + C)/2, level-1) 
      sierpinski(C, (C + A)/2, (C + B)/2, level-1) 
end 