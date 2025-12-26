// Copyright 2016 The Fancy Regex Authors.
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
// THE SOFTWARE.

//! VM builder helper for constructing VM programs.

use alloc::vec::Vec;

use crate::vm::{Insn, Prog};

pub(super) struct VMBuilder {
    pub(super) prog: Vec<Insn>,
    pub(super) n_saves: usize,
}

impl VMBuilder {
    pub(super) fn new(max_group: usize) -> VMBuilder {
        VMBuilder {
            prog: Vec::new(),
            n_saves: max_group * 2,
        }
    }

    pub(super) fn build(self) -> Prog {
        Prog::new(self.prog, self.n_saves)
    }

    pub(super) fn newsave(&mut self) -> usize {
        let result = self.n_saves;
        self.n_saves += 1;
        result
    }

    pub(super) fn pc(&self) -> usize {
        self.prog.len()
    }

    // would "emit" be a better name?
    pub(super) fn add(&mut self, insn: Insn) {
        self.prog.push(insn);
    }

    pub(super) fn set_jmp_target(&mut self, jmp_pc: usize, target: usize) {
        match self.prog[jmp_pc] {
            Insn::Jmp(ref mut next) => *next = target,
            _ => panic!("mutating instruction other than Jmp"),
        }
    }

    pub(super) fn set_split_target(&mut self, split_pc: usize, target: usize, second: bool) {
        match self.prog[split_pc] {
            Insn::Split(_, ref mut y) if second => *y = target,
            Insn::Split(ref mut x, _) => *x = target,
            _ => panic!("mutating instruction other than Split"),
        }
    }

    pub(super) fn set_repeat_target(&mut self, repeat_pc: usize, target: usize) {
        match self.prog[repeat_pc] {
            Insn::RepeatGr { ref mut next, .. }
            | Insn::RepeatNg { ref mut next, .. }
            | Insn::RepeatEpsilonGr { ref mut next, .. }
            | Insn::RepeatEpsilonNg { ref mut next, .. } => *next = target,
            _ => panic!("mutating instruction other than Repeat"),
        }
    }
}
