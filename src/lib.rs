use neon::prelude::*;
use similar::{ChangeTag, TextDiff};
use std::{cell::RefCell, rc::Rc};

#[derive(Clone, Debug)]
enum MoveMut {
    Single(usize),
    Multi(Vec<usize>)
}
impl MoveMut {
    fn get_val(&self) -> Option<usize> {
        match self {
            MoveMut::Single(x) => x.clone().into(),
            _ => None
        }
    }
    fn get_vec(&self) -> Option<Vec<usize>> {
        match self {
            MoveMut::Multi(x) => x.clone().into(),
            _ => None
        }
    }
}

#[derive(Clone, Debug)]
enum InsMut {
    Single(Rc<()>),
    Multi(f64)
}

impl InsMut {
    fn get_ref(&self) -> Option<Rc<()>> {
        match self {
            InsMut::Single(x) => x.clone().into(),
            _ => None
        }
    }
    fn get_val(&self) -> Option<f64> {
        match self {
            InsMut::Multi(x) => x.clone().into(),
            _ => None
        }
    }
}

#[derive(Clone, Debug)]
enum Mutation {
    Del(Rc<String>),
    Ins(InsMut),
    Move(MoveMut),
    Eq(usize)
}

impl Mutation {
    fn is_eq(&self) -> bool {
        match self {
            &Mutation::Eq(_) => true,
            _ => false
        }
    }
    fn get_eq(&self) -> Option<usize> {
        match self {
            &Mutation::Eq(x) => x.into(),
            _ => None
        }
    }
    fn get_rc(&self) -> Option<Rc<String>> {
        match self {
            &Mutation::Del(ref s) => s.clone().into(),
            _ => None
        }
    }
}

#[derive(Debug,Clone)]
enum RestoreResult {
    Eq(String),
    Del(String),
    Ins(String),
    MoveIns(String),
    MoveDel(Rc<RefCell<String>>)
}

impl RestoreResult {
    fn push_str(&mut self, s: &str) {
        match self {
            RestoreResult::Eq(x) => x.push_str(s),
            RestoreResult::Del(x) => x.push_str(s),
            RestoreResult::Ins(x) => x.push_str(s),
            RestoreResult::MoveIns(x) => x.push_str(s),
            RestoreResult::MoveDel(x) => x.borrow_mut().push_str(s)
        }
    }
    fn to_string(&self) -> String {
        match self {
            RestoreResult::Eq(x) => x.clone(),
            RestoreResult::Del(x) => x.clone(),
            RestoreResult::Ins(x) => x.clone(),
            RestoreResult::MoveIns(x) => x.clone(),
            RestoreResult::MoveDel(x) => x.borrow().clone()
        }
    }
    fn substitute(&mut self, s: &str) {
        match self {
            RestoreResult::Eq(x) => {
                x.truncate(0);
                x.push_str(s)
            },
            RestoreResult::Del(x) => {
                x.truncate(0);
                x.push_str(s)
            },
            RestoreResult::Ins(x) => {
                x.truncate(0);
                x.push_str(s)
            },
            RestoreResult::MoveIns(x) => {
                x.truncate(0);
                x.push_str(s)
            },
            RestoreResult::MoveDel(x) => {
                let mut x  = x.borrow_mut();
                x.truncate(0);
                x.push_str(s)
            }
        }
    }
}

enum RestoreResults {
    Plain(String),
    Json(Vec<RestoreResult>)
}

fn _sanitize(s: &str) -> String {
    s.replace('\r', "\n").trim_start_matches(['\n',' ']).trim_end().split('\n').map(|x| if x.trim().len() == 0 { "" } else { x.trim_end() }).collect::<Vec<&str>>().join("\n")
}

fn _str_diff(s0: &str, s1: &str, to_sanitize: bool) -> Vec<Mutation> {
    let (
        s0,
        s1,
        mut out,
        mut del_vec,
        mut ins_vec,
        mut eq,
        mut eq_len,
        mut line_count
    ) = (
        if to_sanitize {_sanitize(s0)} else { s0.to_string() },
        if to_sanitize {_sanitize(s1)} else { s1.to_string() },
        vec![],
        vec![],
        vec![],
        true,
        0,
        0
    );
    let diffs = TextDiff::from_lines(&s0, &s1);
    for change in diffs.iter_all_changes() {
        let mut c = change.to_string();
        c.pop();
        match change.tag() {
            ChangeTag::Delete => {
                if eq && eq_len > 0 {
                    out.push(Mutation::Eq(eq_len));
                    eq_len = 0;
                }
                eq = false;
                if let Some(i) = ins_vec.iter().position(|(x,_)| *x == c ) {
                    let (_,ins_s) = ins_vec.remove(i);
                    let ins_r = out.iter().position(|x| match x {
                        Mutation::Ins(s) => Rc::ptr_eq(&s.get_ref().unwrap(), &ins_s),
                        _ => false
                    }).unwrap();
                    out.remove(ins_r);
                    out.insert(ins_r, Mutation::Move(MoveMut::Single(line_count)));
                    line_count += 1;
                    continue;
                }
                let del_c = Rc::new(c);
                del_vec.iter_mut().for_each(|(_,x,_)| *x = false);
                del_vec.push((del_c.clone(), true, line_count));
                out.push(Mutation::Del(del_c));
                line_count += 1;
            },
            ChangeTag::Insert => {
                if eq && eq_len > 0 {
                    out.push(Mutation::Eq(eq_len));
                    eq_len = 0;
                }
                eq = false;
                if let Some(i) = out.iter().position(|x| match x {
                    Mutation::Del(s) => *s.as_ref() == c,
                    _ => false
                }) {
                    let rc_del = out.remove(i).get_rc().unwrap();
                    let del_pos = del_vec.iter().position(|(x,_,_)| Rc::ptr_eq(x, &rc_del)).unwrap();
                    let del_el = del_vec.remove(del_pos);
                    if del_el.1 {
                        del_vec.iter_mut().for_each(|x| x.1 = false);
                        eq = true;
                        if let Some(el) = out.last() {
                            if el.is_eq() {
                                eq_len = out.pop().unwrap().get_eq().unwrap();
                            }
                        }
                        eq_len += 1;
                        line_count += 1;
                        continue;
                    }
                    if i > 0 {
                        if out[i-1].is_eq() && out[i].is_eq() {
                            let (x0, x1) = (out.remove(i-1).get_eq().unwrap(), out.remove(i-1).get_eq().unwrap());
                            out.insert(i-1, Mutation::Eq(x0+x1));
                        }
                    }
                    out.push(Mutation::Move(MoveMut::Single(del_el.2)));
                } else {
                    let r = Rc::new(());
                    out.push(Mutation::Ins(InsMut::Single(r.clone())));
                    ins_vec.push((c, r));
                }
            },
            ChangeTag::Equal => {
                del_vec.iter_mut().for_each(|x| x.1 = false);
                if let Some(el) = out.last() {
                    if el.is_eq() {
                        eq_len = out.pop().unwrap().get_eq().unwrap();
                    }
                }
                eq = true;
                eq_len += 1;
                line_count += 1;
                continue;
            }
        }
    }
    if eq && eq_len > 0 {
        out.push(Mutation::Eq(eq_len));
    }
    out
}

fn _smart_diff(s0: &str, s1: &str, to_sanitize: bool) -> Vec<Mutation> {
    let (
        muts,
        mut out,
        mut last,
        mut curr_str,
        mut curr_move,
        mut curr_ins
    ) = (
        _str_diff(s0, s1, to_sanitize),
        vec![],
        "",
        "".to_string(),
        vec![],
        vec![]
    );
    for m in muts {
        match m {
            Mutation::Del(s) => {
                if last == "ins" {
                    out.push(Mutation::Ins(InsMut::Multi(curr_ins.len() as f64)));
                    curr_ins.truncate(0);
                } else if last == "move" {
                    out.push(Mutation::Move(if curr_move.len() > 1 {
                        MoveMut::Multi(curr_move.clone())
                    } else {
                        MoveMut::Single(curr_move[0])
                    }));
                    curr_move.truncate(0);
                }
                last = "del";
                curr_str.push_str(&((*s).clone() + "\n"));
            },
            Mutation::Ins(s) => {
                if last == "del" {
                    curr_str.pop();
                    out.push(Mutation::Del(Rc::new(curr_str.clone())));
                    curr_str.truncate(0);
                } else if last == "move" {
                    out.push(Mutation::Move(if curr_move.len() > 1 {
                        MoveMut::Multi(curr_move.clone())
                    } else {
                        MoveMut::Single(curr_move[0])
                    }));
                    curr_move.truncate(0);
                }
                last = "ins";
                curr_ins.push(s);
            },
            Mutation::Move(n) => {
                if last == "del" {
                    curr_str.pop();
                    out.push(Mutation::Del(Rc::new(curr_str.clone())));
                    curr_str.truncate(0);
                } else if last == "ins" {
                    out.push(Mutation::Ins(InsMut::Multi(curr_ins.len() as f64)));
                    curr_ins.truncate(0);
                }
                last = "move";
                curr_move.push(n.get_val().unwrap());
            }
            mm => {
                if last == "del" {
                    curr_str.pop();
                    out.push(Mutation::Del(Rc::new(curr_str.clone())));
                    curr_str.truncate(0);
                } else if last == "ins" {
                    out.push(Mutation::Ins(InsMut::Multi(curr_ins.len() as f64)));
                    curr_ins.truncate(0);
                } else if last == "move" {
                    out.push(Mutation::Move(if curr_move.len() > 1 {
                        MoveMut::Multi(curr_move.clone())
                    } else {
                        MoveMut::Single(curr_move[0])
                    }));
                    curr_move.truncate(0);
                }
                last = "";
                out.push(mm.clone());
            }
        }
    }
    if last == "del" && curr_str.len() > 0 {
        curr_str.pop();
        out.push(Mutation::Del(Rc::new(curr_str.clone())));
    } else if last == "ins" && curr_ins.len() > 0 {
        out.push(Mutation::Ins(InsMut::Multi(curr_ins.len() as f64)));
    } else if last == "move" && curr_move.len() > 0 {
        out.push(Mutation::Move(if curr_move.len() > 1 {
            MoveMut::Multi(curr_move.clone())
        } else {
            MoveMut::Single(curr_move[0])
        }));
    }
    out
}

fn sanitize(mut cx: FunctionContext) -> JsResult<JsString> {
    let s = match cx.argument::<JsString>(0) {
        Ok(s) => s.value(&mut cx),
        Err(_) => return cx.throw_type_error("Argument is not a string")
    };
    Ok(cx.string(_sanitize(&s)))
}

macro_rules! jsonize_muts {
    ($cx:ident, $muts:ident) => {{
        let out_arr = $cx.empty_array();
        for m in $muts {
            let (obj, l) = ($cx.empty_object(), out_arr.len(&mut $cx));
            match m {
                Mutation::Del(v) => {
                    let v = $cx.string((*v).clone());
                    obj.set(&mut $cx, "del", v)?;
                },
                Mutation::Ins(v) => {
                    if let Some(v) = v.get_val() {
                        let v = $cx.number(v);
                        obj.set(&mut $cx, "ins", v)?;
                    } else {
                        let v = $cx.number(1);
                        obj.set(&mut $cx, "ins", v)?;
                    }
                },
                Mutation::Eq(v) => {
                    let v = $cx.number(v.clone() as u32);
                    obj.set(&mut $cx, "eq", v)?;
                },
                Mutation::Move(v) => {
                    if let Some(x) = v.get_val() {
                        let v = $cx.number(x as u32);
                        obj.set(&mut $cx, "move", v)?;
                    } else {
                        let (x, arr) = (v.get_vec().unwrap(), $cx.empty_array());
                        for i in x {
                            let (v, l) = ($cx.number(i as u32), arr.len(&mut $cx));
                            arr.set(&mut $cx, l, v)?;
                        }
                        obj.set(&mut $cx, "move", arr)?;
                    };
                }
            }
            out_arr.set(&mut $cx, l, obj)?;
        }
        out_arr
    }};
}

fn str_diff(mut cx: FunctionContext) -> JsResult<JsArray> {
    let (s0, s1, to_sanitize) = (match cx.argument::<JsString>(0) {
        Ok(s) => s.value(&mut cx),
        Err(_) => return cx.throw_type_error("Argument 0 is not a string")
    }, match cx.argument::<JsString>(1) {
        Ok(s) => s.value(&mut cx),
        Err(_) => return cx.throw_type_error("Argument 1 is not a string")
    }, match cx.argument_opt(2) {
        Some(b) => match b.downcast::<JsBoolean,_>(&mut cx) {
            Ok(b) => b.value(&mut cx),
            Err(_) => return cx.throw_type_error("Argument 2 is not a boolean")
            
        },
        None => false
    });
    let muts = _str_diff(&s0, &s1, to_sanitize);
    Ok(jsonize_muts!(cx, muts))
}

fn smart_diff(mut cx: FunctionContext) -> JsResult<JsArray> {
    let (s0, s1, to_sanitize) = (match cx.argument::<JsString>(0) {
        Ok(s) => s.value(&mut cx),
        Err(_) => return cx.throw_type_error("Argument 0 is not a string")
    }, match cx.argument::<JsString>(1) {
        Ok(s) => s.value(&mut cx),
        Err(_) => return cx.throw_type_error("Argument 1 is not a string")
    }, match cx.argument_opt(2) {
        Some(b) => match b.downcast::<JsBoolean,_>(&mut cx) {
            Ok(b) => b.value(&mut cx),
            Err(_) => return cx.throw_type_error("Argument 2 is not a boolean")
            
        },
        None => false
    });
    let muts = _smart_diff(&s0, &s1, to_sanitize);
    Ok(jsonize_muts!(cx, muts))
}



fn _restore(s: &str, muts: &[Mutation], sanitize: bool, to_json: bool) -> RestoreResults {
    let s = if sanitize {_sanitize(s)} else { s.to_string() };
    let (
        mut out_vec,
        mut out_struct,
        mut to_move,
        mut line_count,
        mut move_del
    ) = (
        s.split('\n').map(|s| s.to_string()).collect::<Vec<String>>(),
        vec![],
        vec![],
        0,
        vec![]
    );
    muts.iter().for_each(|x| match x {
        Mutation::Move(v) => if let Some(n) = v.get_val() {
            move_del.push((false, n, Some(RestoreResult::MoveDel(Rc::new(RefCell::new("".to_string()))))))
        } else {
            v.get_vec().unwrap().iter().for_each(|n| move_del.push((false, *n, Some(RestoreResult::MoveDel(Rc::new(RefCell::new("".to_string())))))))
        },
        _ => ()
    });
    if move_del.len() > 0 {
        move_del.sort_by(|&(_,a,_), &(_,b,_)|a.cmp(&b));
        let mut next_ind = 0;
        let mut move_del_s = move_del.get(next_ind).unwrap().2.as_ref().unwrap().clone();
        move_del.iter_mut().for_each(|x|{
            if next_ind != 0 && next_ind == x.1 {
                x.2 = None;
            } else {
                move_del_s = x.2.as_ref().unwrap().clone();
            }
            move_del_s.push_str("\n");
            next_ind = x.1 + 1;
        });
    }
    for m in muts {
        match m {
            Mutation::Del(s) => {
                let mut cs = "".to_string();
                s.split('\n').map(|s| s.to_string()).for_each(|s| {
                    if let Some(mi) = move_del.iter_mut().position(|x|x.1 == line_count && !x.0) {
                        let (mut last_mi, mut next_mi) = (mi, mi+1);
                        move_del[mi].0 = true;
                        while move_del[last_mi].2.is_none() {
                            last_mi -= 1;
                            move_del[last_mi].0 = true;
                        }
                        while move_del.get(next_mi).is_some() && move_del[next_mi].2.is_none() {
                            move_del[next_mi].0 = true;
                            next_mi += 1;
                        }
                        if cs.len() > 0 {
                            out_struct.push(RestoreResult::Del(cs.clone()));
                            cs.truncate(0);
                        }
                        out_struct.push(move_del[last_mi].2.as_ref().unwrap().clone());
                    }
                    cs.push_str(&(s.clone() + "\n"));
                    out_vec.insert(line_count, s.to_string());
                    line_count += 1;
                });
                out_struct.push(RestoreResult::Del(cs));
            },
            Mutation::Ins(i) => {
                if let Some(mi) = move_del.iter_mut().position(|x|x.1 == line_count && !x.0) {
                    let (mut last_mi, mut next_mi) = (mi, mi+1);
                    move_del[mi].0 = true;
                    while move_del[last_mi].2.is_none() {
                        last_mi -= 1;
                        move_del[last_mi].0 = true;
                    }
                    while move_del.get(next_mi).is_some() && move_del[next_mi].2.is_none() {
                        move_del[next_mi].0 = true;
                        next_mi += 1;
                    }
                    out_struct.push(move_del[last_mi].2.as_ref().unwrap().clone());
                }
                let i = i.get_val().unwrap() as usize;
                let mut r = RestoreResult::Ins("".to_string());
                for _ in 0..i {
                    let s = out_vec.remove(line_count);
                    r.push_str(&(s + "\n"));
                }
                out_struct.push(r);
            },
            Mutation::Move(m) => {
                if let Some(mi) = move_del.iter_mut().position(|x|x.1 == line_count && !x.0) {
                    let (mut last_mi, mut next_mi) = (mi, mi+1);
                    move_del[mi].0 = true;
                    while move_del[last_mi].2.is_none() {
                        last_mi -= 1;
                        move_del[last_mi].0 = true;
                    }
                    while move_del.get(next_mi).is_some() && move_del[next_mi].2.is_none() {
                        move_del[next_mi].0 = true;
                        next_mi += 1;
                    }
                    out_struct.push(move_del[last_mi].2.as_ref().unwrap().clone());
                }
                if let Some(n) = m.get_val() {
                    let s = out_vec.remove(line_count);
                    let mut last_mi = n;
                    let mut last_m = move_del.iter_mut().find(|x| x.1 == last_mi).unwrap();
                    while last_m.2.is_none() {
                        last_mi -= 1;
                        last_m = move_del.iter_mut().find(|x| x.1 == last_mi).unwrap();
                    }
                    let m_pos = last_mi - n;
                    let o_s = last_m.2.as_ref().unwrap().to_string();
                    let mut ns = "".to_string();
                    o_s.split('\n').into_iter().enumerate().for_each(|(i, ss)| {
                        if m_pos == i {
                            ns.push_str(&(s.clone() + "\n"));
                        } else {
                            ns.push_str(&(ss.to_string() + "\n"));
                        }
                    });
                    last_m.2.as_mut().unwrap().substitute(&ns);
                    out_struct.push(RestoreResult::MoveIns(s.clone() + "\n"));
                    to_move.push((n, s));
                } else {
                    let mut r = RestoreResult::MoveIns("".to_string());
                    m.get_vec().unwrap().iter().for_each(|n| {
                        let s = out_vec.remove(line_count);
                        let mut last_mi = *n;
                        let mut last_m = move_del.iter_mut().find(|x| x.1 == last_mi).unwrap();
                        while last_m.2.is_none() {
                            last_mi -= 1;
                            last_m = move_del.iter_mut().find(|x| x.1 == last_mi).unwrap();
                        }
                        let m_pos = n - last_mi;
                        let o_s = last_m.2.as_ref().unwrap().to_string();
                        let mut os_split = o_s.split('\n').into_iter().map(|x| x.to_string()).collect::<Vec<String>>();
                        os_split.remove(m_pos);
                        os_split.insert(m_pos, s.clone());
                        last_m.2.as_mut().unwrap().substitute(&os_split.join("\n"));
                        r.push_str(&(s.clone() + "\n"));
                        to_move.push((*n, s));
                    });
                    out_struct.push(r);
                }
            },
            Mutation::Eq(n) => {
                let mut cs ="".to_string();
                for i in line_count..line_count+n {
                    if let Some(mi) = move_del.iter_mut().position(|x|x.1 == line_count && !x.0) {
                        let (mut last_mi, mut next_mi) = (mi, mi+1);
                        move_del[mi].0 = true;
                        while move_del[last_mi].2.is_none() {
                            last_mi -= 1;
                            move_del[last_mi].0 = true;
                        }
                        while move_del.get(next_mi).is_some() && move_del[next_mi].2.is_none() {
                            move_del[next_mi].0 = true;
                            next_mi += 1;
                        }
                        if cs.len() > 0 {
                            out_struct.push(RestoreResult::Eq(cs.clone()));
                            cs.truncate(0);
                        }
                        out_struct.push(move_del[last_mi].2.as_ref().unwrap().clone());
                    }
                    cs.push_str(&(out_vec[i].clone() + "\n"));
                    line_count += 1;
                }
                out_struct.push(RestoreResult::Eq(cs));
            }
        }
    }
    if to_json {
        return RestoreResults::Json(out_struct);
    }
    to_move.sort_by(|(a,_),(b, _)| a.cmp(b));
    to_move.iter().for_each(|(i, s)| out_vec.insert(*i, s.to_string()));
    RestoreResults::Plain(out_vec.join("\n").to_string())
}



macro_rules! rustify_muts {
    ($cx:ident, $muts:ident) => {{
        let mut o = vec![];
        for m in $muts {
            let obj = m.downcast::<JsObject,_>(&mut $cx).unwrap();
            o.push(
                if let Ok(v) = obj.get_value(&mut $cx, "del")?.downcast::<JsString,_>(&mut $cx) {
                    Mutation::Del(Rc::new(v.value(&mut $cx)))
                } else if let Ok(v) = obj.get_value(&mut $cx, "ins")?.downcast::<JsNumber,_>(&mut $cx) {
                    Mutation::Ins(InsMut::Multi(v.value(&mut $cx)))
                } else if let Ok(v) = obj.get_value(&mut $cx, "move")?.downcast::<JsNumber,_>(&mut $cx) {
                    Mutation::Move(MoveMut::Single(v.value(&mut $cx) as usize))
                } else if let Ok(v) = obj.get_value(&mut $cx, "move")?.downcast::<JsArray,_>(&mut $cx) {
                    Mutation::Move(MoveMut::Multi(
                        v.to_vec(&mut $cx)?.iter()
                        .map(|x| x.downcast::<JsNumber,_>(&mut $cx).unwrap().value(&mut $cx) as usize )
                        .collect::<Vec<usize>>()
                    ))
                } else {
                    let v = obj.get::<JsNumber,_,_>(&mut $cx, "eq")?;
                    Mutation::Eq(v.value(&mut $cx) as usize)
                }
            );
        }
        o
    }};
}

fn compare(mut cx: FunctionContext) -> JsResult<JsValue> {
    let (s0, s1, to_sanitize) = (match cx.argument::<JsString>(0) {
        Ok(s) => s.value(&mut cx),
        Err(_) => return cx.throw_type_error("Argument 0 is not a string")
    }, match cx.argument::<JsString>(1) {
        Ok(s) => s.value(&mut cx),
        Err(_) => return cx.throw_type_error("Argument 1 is not a string")
    }, match cx.argument_opt(2) {
        Some(b) => match b.downcast::<JsBoolean,_>(&mut cx) {
            Ok(b) => b.value(&mut cx),
            Err(_) => return cx.throw_type_error("Argument 2 is not a boolean")
            
        },
        None => false
    });
    let muts = _smart_diff(&s0, &s1, to_sanitize);
    Ok(match _restore(&s1, &muts, to_sanitize, true) {
        RestoreResults::Json(v) => {
            let r = cx.empty_array();
            for x in v.iter() {
                let (o, l) = (cx.empty_object(), r.len(&mut cx));
                match &x {
                    RestoreResult::Del(s) => {
                        let s = cx.string(s);
                        o.set(&mut cx, "del", s)?
                    },
                    RestoreResult::Ins(s) => {
                        let s = cx.string(s);
                        o.set(&mut cx, "ins", s)?
                    },
                    RestoreResult::MoveDel(s) => {
                        let s = cx.string(s.borrow().to_string());
                        o.set(&mut cx, "moveDel", s)?
                    },
                    RestoreResult::MoveIns(s) => {
                        let s = cx.string(s);
                        o.set(&mut cx, "moveIns", s)?
                    },
                    RestoreResult::Eq(s) => {
                        let s = cx.string(s);
                        o.set(&mut cx, "eq", s)?
                    }
                };
                r.set(&mut cx, l, o)?;
            }
            r.upcast()
        }
        _ => cx.undefined().upcast()
    })
}

fn restore(mut cx: FunctionContext) -> JsResult<JsValue> {
    let (s, muts, sanitize) = (match cx.argument::<JsString>(0) {
        Ok(s) => s.value(&mut cx),
        Err(_) => return cx.throw_type_error("Argument 0 is not a string")
    }, match cx.argument::<JsArray>(1) {
        Ok(v) => v.to_vec(&mut cx)?,
        Err(_) => return cx.throw_type_error("Argument 1 is not an array")
    }, match cx.argument_opt(2) {
        Some(b) => match b.downcast::<JsBoolean,_>(&mut cx) {
            Ok(b) => b.value(&mut cx),
            Err(_) => return cx.throw_type_error("Argument 2 is not a boolean")
            
        },
        None => false
    });
    Ok(match _restore(&s, &rustify_muts!(cx, muts), sanitize, false) {
        RestoreResults::Plain(s) => cx.string(s).upcast(),
        _ => cx.undefined().upcast()
    })
}

#[neon::main]
fn main(mut cx: ModuleContext) -> NeonResult<()> {
    cx.export_function("sanitize", sanitize)?;
    cx.export_function("strDiff", str_diff)?;
    cx.export_function("diff", smart_diff)?;
    cx.export_function("restore", restore)?;
    cx.export_function("compare", compare)?;
    Ok(())
}