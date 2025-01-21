// for RV32F
//test_0121
package Pipeline
import chisel3._
import chisel3.util._
import FPUOpCode._


object FPUOpCode {
  // 
  val FPU_FADD_S = 0.U(5.W)
  val FPU_FSUB_S = 1.U(5.W)
  val FPU_FMUL_S = 2.U(5.W)
  val FPU_FDIV_S = 3.U(5.W)
  val FPU_FSQRT_S = 11.U(5.W)
  val FPU_FSGN_S = 4.U(5.W)
  val FPU_FMIN_S = 5.U(5.W)
  val FPU_FMAX_S = 5.U(5.W)
  val FPU_FCVT_W_S = 24.U(5.W)
  val FPU_FMV_S = 28.U(5.W)
  val FPU_FCLASS_S = 28.U(5.W)
  val FPU_FEQ_S = 20.U(5.W)
  val FPU_FLT_S = 20.U(5.W)   
  val FPU_FLE_S = 20.U(5.W)
  val FPU_FCVT_S_W = 26.U(5.W)
  val FPU_FMV_S_X = 30.U(5.W)

  val FPU_FMADD_S = 16.U(5.W)
  val FPU_FMSUB_S = 17.U(5.W)
  val FPU_FNMSUB_S = 18.U(5.W)
  val FPU_FNMADD_S = 19.U(5.W)

  // GPT
  // val FPU_FADD_S = 0.U(5.W)
  // val FPU_FSUB_S = 1.U(5.W)
  // val FPU_FMUL_S = 2.U(5.W)
  // val FPU_FDIV_S = 3.U(5.W)
  // val FPU_FSGNJ_S = 4.U(5.W)
  // val FPU_FSGNJN_S = 5.U(5.W)
  // val FPU_FSGNJX_S = 6.U(5.W)
  // val FPU_FMIN_S = 7.U(5.W)
  // val FPU_FMAX_S = 8.U(5.W)
  // val FPU_FEQ_S = 9.U(5.W)
  // val FPU_FLT_S = 10.U(5.W)   
  // val FPU_FLE_S = 11.U(5.W)
  // val FPU_FCVT_W_S = 12.U(5.W)
  // val FPU_FCVT_WU_S = 13.U(5.W)
  // val FPU_FMV_X_W = 14.U(5.W)
  // val FPU_FCLASS_S = 15.U(5.W)
  // val FPU_FCVT_S_W = 16.U(5.W)
  // val FPU_FCVT_S_WU = 17.U(5.W)
  // val FPU_FMV_W_X = 18.U(5.W)
}

class FPU extends Module{
  val io = IO(new Bundle{
      val A_data_in = Input(UInt(32.W))
      val B_data_in = Input(UInt(32.W))
      val C_data_in = Input(UInt(32.W))
      val COMP = Input(Bool())
      val fpu_Op = Input(UInt(5.W))
      val out = Output(UInt(32.W))
  })

  //reg type or wire type?
  val result = 0.U(32.W)
  val result_sign = 0.U(1.W)
  val result_exp = 0.U(8.W)
  val result_frac = 0.U(23.W)
  val carry = 0.U(1.W)
  val exp_diff = 0.U(8.W)

  val A_sign = 0.U(1.W)
  val B_sign = 0.U(1.W)
  val A_exp = 0.U(1.W)
  val B_exp = 0.U(1.W)
  val A_Mantissa = 0.U(24.W)
  val B_Mantissa = 0.U(24.W)
  val Temp_Exp = 0.U(24.W)
  val Temp_Mantissa = 0.U(24.W)
  val B_shift_mantissa = 0.U(24.W)

  //comparator
  FP_COMP.io.rs1 := io.A_data_in
  FP_COMP.io.rs2 := io.B_data_in
  COMP := FP_COMP.io.COMP_RESULT  

  val A_swap = COMP ? io.A_data_in : io.B_data_in
  val B_swap = COMP ? io.B_data_in : io.A_data_in

  switch(io.fpu_Op) {
    is(FPU_FADD_S) {
      A_Mantissa := Cat(1.U(1.W), A_swap(22, 0))
      B_Mantissa := Cat(1.U(1.W), B_swap(22, 0))
      // shift mantissa
      exp_diff := A_swap(30, 23) - B_swap(30, 23)
      // add the two mantissa numbers and store the carry
      B_shift_mantissa := B_Mantissa >> exp_diff
      Temp_Mantissa := (A_swap(31) ^ B_swap(31)) ? A_Mantissa - B_shift_mantissa : A_Mantissa + B_shift_mantissa
      carry := Temp_Mantissa(24)
      // normalize the result
      //rd??
      when(carry) {
        Temp_Mantissa := Temp_Mantissa >> 1
        when(result_exp < 255.U) {
          result_exp := A_swap(30:23) + 1.U
          // result_exp := result_exp + 1.U
        } .otherwise {
          result_exp := 255.U
        }
      } .elsewhen(Temp_Mantissa =/= 1.U) {
        Temp_Mantissa := 0.U
      } .otherwise {
        for(i <- 0 until 24) {
          when(Temp_Mantissa(23) =/= 1.U && result_exp > 0.U) {
            Temp_Mantissa := Temp_Mantissa << 1
            result_exp := result_exp - 1.U
          }
        }
      }
      result_sign := A_swap(31)
      result_frac := Temp_Mantissa(22, 0)
      result := Cat(result_sign, result_exp, result_frac)
      //result_exp := A_swap(30, 23) + carry - 127.U
      //result := io.in_A + io.in_B
    }

    is(FPU_FSUB_S) {
      A_Mantissa := Cat(1.U(1.W), A_swap(22, 0))
      B_Mantissa := Cat(1.U(1.W), B_swap(22, 0))
      // shift mantissa
      exp_diff := A_swap(30, 23) - B_swap(30, 23)
      // add the two mantissa numbers and store the carry
      B_shift_mantissa := B_Mantissa >> exp_diff
      Temp_Mantissa := (A_swap(31) ^ B_swap(31)) ? A_Mantissa + B_shift_mantissa : A_Mantissa - B_shift_mantissa
      carry := Temp_Mantissa(24)

      when(carry) {
        Temp_Mantissa := Temp_Mantissa >> 1
        when(result_exp < 255.U) {
          result_exp := A_swap(30:23) + 1.U 
        } .otherwise {
          result_exp := 255.U
        }
      } .elsewhen(Temp_Mantissa =/= 1.U) {
        Temp_Mantissa := 0.U
      } .otherwise {
        for(i <- 0 until 24) {
          when(Temp_Mantissa(23) =/= 1.U && result_exp > 0.U) {
            Temp_Mantissa := Temp_Mantissa << 1
            result_exp := result_exp - 1.U
          }
        }
      }
      result_sign := A_swap(31)
      result_frac := Temp_Mantissa(22, 0)
      result := Cat(result_sign, result_exp, result_frac)
      //result := io.in_A - io.in_B
    }
    is(FPU_FMUL_S) {
      // extract the sign bit, exponent, and mantissa
      A_sign = A_swap(31)
      B_sign = B_swap(31)
      A_exp = A_swap(30, 23)
      B_exp = B_swap(30, 23)
      A_Mantissa = Cat(1.U(1.W), A_swap(22, 0))
      B_Mantissa = Cat(1.U(1.W), B_swap(22, 0))
      // multiply the signs
      result_sign = A_sign ^ B_sign
      // add the exponents
      val exp_sum = A_exp + B_exp - 127.U
      // multiply the mantissa
      val Mantissa_product = A_Mantissa * B_Mantissa
      // normalize
      val carry = Mantissa_product(47)
      val normalized_Mantissa = Mux(carry, Mantissa_product(46, 24), Mantissa_product(45, 23))
      val normalized_exp = Mux(carry, exp_sum + 1.U, exp_sum)

      // exponent overflow or zero mantissa
      when(Mantissa_product === 0.U) {
        result_exp := 0.U
        result_frac := 0.U
      } .elsewhen(normalized_exp >= 255.U) {
        result_exp := 255.U // infinity
        result_frac := 0.U
      } .elsewhen(normalized_exp <= 0.U) {
        result_exp := 0.U // subnormal number
        result_frac := normalized_Mantissa >> (1.U - normalized_exp)
      } .otherwise {
        result_exp := normalized_exp
        result_frac := normalized_Mantissa
      }

      // final result
      result := Cat(result_sign, result_exp, result_frac)
      
      // // multiply the signs
      // val result_sign = rs1_sign ^ rs2_sign
      // // multiply the fractions
      // val result_frac = rs1_frac * rs2_frac
      // val exp_update = Mux(result_frac(47), 1.U, 0.U)
      // // add the exponents
      // val result_exp = rs1_exp + rs2_exp + exp_update - 127.U
      // //???
    }
    
    is(FPU_FSQRT_S) {
      // extract the sign bit, exponent, and mantissa
      A_sign = A_swap(31)
      A_exp = A_swap(30, 23)
      A_Mantissa = Cat(1.U(1.W), A_swap(22, 0))

      when(A_sign) {
        // when input negative, output NaN
        result := "b01111111100000000000000000000000".U
      } .elsewhen(A_swap(30, 0) === 0.U) {
        // when input zero, output zero
        result := A_swap
      } .otherwise {
        // exponent
        val exp_adjust = A_exp - 127.U 
        // odd or even number
        val new_exp = Mux(exp_adjust(0),
          (exp_adjust >> 1) + 127.U,
          ((exp_adjust - 1.U) >> 1) + 127.U
        )
        // Newton Iteration(3 times)
        val x0 = "b01000000000000000000000000000000".U(32.W) // x0 assumption = 1
        val iter1 = (x0 + (A_Mantissa << 23) / x0) >> 1
        val iter2 = (iter1 + (A_Mantissa << 23) / iter1) >> 1
        val iter3 = (iter2 + (A_Mantissa << 23) / iter2) >> 1
        val sqrt_Mantissa = iter2(45, 23) 

        // final result
        result_sign := 0.U 
        result_exp := new_exp
        result_frac := sqrt_Mantissa
        result := Cat(result_sign, result_exp, result_frac)
      }
    }
    
    is(FPU_FDIV_S) {
      // extract the sign bit, exponent, and mantissa
      A_sign := A_swap(31)
      B_sign := B_swap(31)
      A_exp := A_swap(30, 23)
      B_exp := B_swap(30, 23)
      A_Mantissa := Cat(1.U(1.W), A_swap(22, 0))
      B_Mantissa := Cat(1.U(1.W), B_swap(22, 0))

      // divide the signs
      val result_sign = A_sign ^ B_sign
      // minus the exponents
      val exp_diff = A_exp - B_exp + 127.U

      // Newton Iteration(3 times) 1 / B_Mantissa
      val x0 = "b01000000000000000000000000000000".U(32.W) // x0 assumption = 1
      val iter1 = (x0 + (1.U << 23) - ((B_Mantissa << 23) / x0)) >> 1
      val iter2 = (iter1 + (1.U << 23) - ((B_Mantissa << 23) / iter1)) >> 1
      val reciprocal = iter2(45, 23) // 提取尾數高位作為 1 / B_Mantissa 的結果

      // 最終尾數計算 A_Mantissa / B_Mantissa = A_Mantissa * reciprocal
      val Mantissa_div = A_Mantissa * reciprocal

      // 5. 歸一化
      val carry = Mantissa_div(47)
      val normalized_Mantissa = Mux(carry, Mantissa_div(46, 24), Mantissa_div(45, 23))
      val normalized_exp = Mux(carry, exp_diff + 1.U, exp_diff)

      // 6. 特殊情況處理
      when(B_swap(30, 0) === 0.U) {
        // 零除返回 NaN
        result := "b01111111100000000000000000000000".U
      } .elsewhen(A_swap(30, 0) === 0.U) {
        // A 為零，結果為零
        result := 0.U
      } .elsewhen(normalized_exp >= 255.U) {
        // 指數溢出，結果為 Infinity
        result_exp := 255.U
        result_frac := 0.U
        result := Cat(result_sign, result_exp, result_frac)
      } .elsewhen(normalized_exp <= 0.U) {
        // 非規範化數
        result_exp := 0.U
        result_frac := normalized_Mantissa >> (1.U - normalized_exp)
        result := Cat(result_sign, result_exp, result_frac)
      } .otherwise {
        
        // final result
        result_exp := normalized_exp
        result_frac := normalized_Mantissa
        result := Cat(result_sign, result_exp, result_frac)
      }
    }



    is(FPU_FSGNJ_S) {
      //result := Cat(io.in_A(31), io.in_B(30, 0))
    }
    is(FPU_FSGNJN_S) {
      //result := Cat(~io.in_A(31), io.in_B(30, 0))
    }
    is(FPU_FSGNJX_S) {
      //result := Cat(io.in_A(31) ^ io.in_B(31), io.in_B(30, 0))
    }
    is(FPU_FMIN_S) {
      //result := Mux(io.in_A < io.in_B, io.in_A, io.in_B)
    }
    is(FPU_FMAX_S) {
      //result := Mux(io.in_A > io.in_B, io.in_A, io.in_B)
    }
    is(FPU_FEQ_S) {
      //result := Mux(io.in_A === io.in_B, 1.U, 0.U)
    }
    is(FPU_FLT_S) {
      //result := Mux(io.in_A < io.in_B, 1.U, 0.U)
    }
    is(FPU_FLE_S) {
      //result := Mux(io.in_A <= io.in_B, 1.U, 0.U)
    }
    is(FPU_FCVT_W_S) {
      //result := io.in_A.asSInt
    }
    is(FPU_FCVT_WU_S, FPU_FMV_X_W, FPU_FCVT_S_WU, FPU_FMV_W_X) {
      //result := io.in_A
    }
    is(FPU_FCLASS_S) {
      //result := 0.U
    }
    is(FPU_FCVT_S_W) {
      //result := io.in_A.asUInt
    }
  }
}

/*
fp:
  [31] sign
  [30:23] exp
  [22:0] frac

fp_add: 
  swap exp and frac
  extend exp to 24 bits
  shift frac to the right by 1
  add the two numbers
  normalize the result

fp_mul:
  add the exponents
  multiply the fractions
  normalize the result

  
***NOTICE*** : rounding mode should be considered
 */