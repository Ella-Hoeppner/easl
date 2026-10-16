//! Dependency graphs over nodes numbered `0..n`, given as each node's
//! dependencies.

use std::cmp::Reverse;
use std::collections::BinaryHeap;

/// Orders nodes `0..dependencies.len()` so each comes after every node in
/// its `dependencies` entry, taking the lowest-numbered ready node at each
/// step: nodes keep their numbering's order wherever dependencies allow.
/// When some can't be ordered, returns the graph's [`cycles`] instead.
pub fn topological_order(
  dependencies: &[Vec<usize>],
) -> Result<Vec<usize>, Vec<Vec<usize>>> {
  let mut unmet: Vec<usize> = dependencies.iter().map(Vec::len).collect();
  let mut dependents: Vec<Vec<usize>> = vec![vec![]; dependencies.len()];
  for (node, node_dependencies) in dependencies.iter().enumerate() {
    for dependency in node_dependencies {
      dependents[*dependency].push(node);
    }
  }
  let mut ready: BinaryHeap<Reverse<usize>> = (0..dependencies.len())
    .filter(|node| unmet[*node] == 0)
    .map(Reverse)
    .collect();
  let mut order = Vec::with_capacity(dependencies.len());
  while let Some(Reverse(node)) = ready.pop() {
    order.push(node);
    for dependent in &dependents[node] {
      unmet[*dependent] -= 1;
      if unmet[*dependent] == 0 {
        ready.push(Reverse(*dependent));
      }
    }
  }
  if order.len() == dependencies.len() {
    Ok(order)
  } else {
    Err(cycles(dependencies))
  }
}

/// The dependency cycles found by a depth-first walk from each node in
/// order, one per dependency that leads back to a node on the walk's path:
/// each lists the nodes along it, every one depending on the next and the
/// last on the first.
pub fn cycles(dependencies: &[Vec<usize>]) -> Vec<Vec<usize>> {
  #[derive(Clone, Copy, PartialEq)]
  enum State {
    Unvisited,
    Active,
    Done,
  }
  fn visit(
    node: usize,
    dependencies: &[Vec<usize>],
    states: &mut [State],
    path: &mut Vec<usize>,
    cycles: &mut Vec<Vec<usize>>,
  ) {
    states[node] = State::Active;
    path.push(node);
    for dependency in &dependencies[node] {
      match states[*dependency] {
        State::Unvisited => {
          visit(*dependency, dependencies, states, path, cycles)
        }
        State::Active => {
          let start = path.iter().position(|n| n == dependency).unwrap();
          cycles.push(path[start..].to_vec());
        }
        State::Done => {}
      }
    }
    path.pop();
    states[node] = State::Done;
  }
  let mut states = vec![State::Unvisited; dependencies.len()];
  let mut cycles = vec![];
  for node in 0..dependencies.len() {
    if states[node] == State::Unvisited {
      visit(node, dependencies, &mut states, &mut vec![], &mut cycles);
    }
  }
  cycles
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn orders_dependencies_first_then_by_number() {
    // 0 needs 2; 1 and 2 are free.
    assert_eq!(
      topological_order(&[vec![2], vec![], vec![]]),
      Ok(vec![1, 2, 0])
    );
    // A chain written backwards.
    assert_eq!(
      topological_order(&[vec![1], vec![2], vec![]]),
      Ok(vec![2, 1, 0])
    );
    // Repeated dependencies count once each way.
    assert_eq!(topological_order(&[vec![1, 1], vec![]]), Ok(vec![1, 0]));
  }

  #[test]
  fn reports_each_cycle() {
    // 0 <-> 1, 2 on itself, 3 -> 4 -> 3, and 5 only depends on a cycle.
    assert_eq!(
      topological_order(&[
        vec![1],
        vec![0],
        vec![2],
        vec![4],
        vec![3],
        vec![0],
      ]),
      Err(vec![vec![0, 1], vec![2], vec![3, 4]])
    );
  }
}
