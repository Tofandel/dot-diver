/* -------------------------------------------------------------------------- */
/*                                Utility Types                               */
/* -------------------------------------------------------------------------- */

// check if a type is null or undefined
type IsNullableOrUndefineable<T> = null extends T ? true : undefined extends T ? true : false

// Removes all possible index signatures from a type
type FilterIndexSignatureType<T> = string extends T
  ? never
  : number extends T
  ? never
  : symbol extends T
  ? never
  : T

// Remove the index signature from a type
type RemoveIndexSignature<T> = {
  [K in keyof T as FilterIndexSignatureType<K>]: T[K]
}

// check if a type has an index signature
type HasIndexSignature<T> = string extends keyof T
  ? true
  : number extends keyof T
  ? true
  : symbol extends keyof T
  ? true
  : false

type GetValidIndexSignature<T> = Exclude<keyof T, symbol> extends infer K
  ? K extends string | number
    ? K
    : never
  : never

// Keys have to be strings or number and can't contain a dot since we won't be able to differ between and key with a dot and a nested key
type RemoveInvalidDotPathKeys<T> = T extends symbol
  ? never
  : T extends number
  ? T
  : T extends string
  ? // eslint-disable-next-line @typescript-eslint/no-unused-vars
    T extends `${infer _K}.${infer _R}`
    ? never
    : T
  : never

// check if a type is an array
type IsArray<T> = T extends Array<any> ? true : false

// check if a array is empty
type IsEmptyArray<T> = T extends [] ? true : false

// Get the type of an array element
type GetArrayElement<T> = T extends Array<infer U> ? U : never

/**
 * check if a type is any
 * @link https://stackoverflow.com/a/49928360/1490091
 */
type IsAny<T> = 0 extends 1 & T ? true : false

// check if a type is never
type IsNever<T> = [T] extends [never] ? true : false

/**
 * check if a type is unknown
 * @link https://github.com/sindresorhus/type-fest
 */
type IsUnknown<T> = IsNever<T> extends true
  ? false
  : IsAny<T> extends true
  ? false
  : unknown extends T
  ? true
  : false

// check if a type is a primitive
type IsPrimitive<T> = T extends string | number | boolean | bigint | symbol | undefined | null
  ? true
  : false

// remove null and undefined from a type
type ExcludeNullUndefined<T> = Exclude<T, null | undefined>

// check if a type is a tuple
type IsTuple<T> = T extends [any, ...any[]] ? true : false

// get the length of a tuple
type TupleLength<T> = T extends { length: infer L } ? (L extends number ? L : never) : never

// get the type of a tuple element
type TupleElement<T, N> = N extends keyof T ? T[N] : never

// remove readonly from members of a record
type Writeable<T> = {
  -readonly [K in keyof T]: T[K]
}

type BeforeLast<T extends string, V extends string | number> = T extends string
  ? T extends `${infer H}${V}${infer R}`
    ? R extends `${string}${V}${string}`
      ? `${H}${V}${BeforeLast<R, V>}`
      : H
    : T
  : never

/* -------------------------------------------------------------------------- */
/*                                 Math Types                                 */
/* -------------------------------------------------------------------------- */
// We use a tuple to carry our value

type MinusOne<N extends unknown[]> = N extends [...infer U, unknown] ? U : never

type BuildTuple<L extends number, T extends unknown[] = []> = T extends { length: L }
  ? T
  : BuildTuple<L, [...T, unknown]>

/* -------------------------------------------------------------------------- */
/*                                 Path Types                                 */
/* -------------------------------------------------------------------------- */

// get all possible paths of a record
type GetRecordPaths<
  T,
  DepthCarry extends unknown[],
  K extends keyof T = keyof T
> = K extends keyof T
  ?
      | RemoveInvalidDotPathKeys<K>
      | `${RemoveInvalidDotPathKeys<K>}.${TraversalGate<T[K], DepthCarry>}`
  : never

// get all possible paths of an array
type GetArrayPaths<T, DepthCarry extends unknown[]> = `\${number}.${TraversalGate<
  GetArrayElement<T>,
  DepthCarry
>}`

// get all possible paths of a tuple
type GetTuplePaths<T extends unknown[], DepthCarry extends unknown[]> = Extract<
  keyof T,
  `${number}`
> extends infer P
  ? P extends `${infer PV extends number}`
    ? PV | `${PV}.${TraversalGate<TupleElement<T, PV>, DepthCarry>}`
    : never
  : never

/* -------------------------------------------------------------------------- */
/*                                 Simple Path                                */
/* -------------------------------------------------------------------------- */
/**
 * Simple Path return all paths in the given type and returns them until it reaches its max depth.
 * It works by recursively traversing each property of the current object until it reaches a primitive.
 *
 * Since Typescript could include cyclic types, e.g.
 *
 * type CyclicType = {
 *  cycle: CyclicType
 *  ...
 * }
 *
 * we limit the depth. We can archive this by constructing a tuple the size of the max depth we want to go,
 * and removing one element whenever we go one level deeper. As soon as the tuple is empty, we stop traversing,
 *
 *
 */

/**
 * Checks the given generic Parameter T for its type
 * and chooses an appropiate traversal strategy to find further paths.
 *
 * @typeParam T - Type to check
 * @typeParam DepthCarry - Tuple that is the size of the remaining depth level
 */
type TraversalStep<T, DepthCarry extends unknown[]> = IsAny<T> extends true
  ? string
  : IsUnknown<T> extends true
  ? never
  : IsPrimitive<T> extends true
  ? never
  : IsTuple<T> extends true
  ? T extends unknown[]
    ? GetTuplePaths<T, DepthCarry>
    : never
  : IsArray<T> extends true
  ? (IsEmptyArray<T> extends true ? never : number) | GetArrayPaths<T, DepthCarry>
  : HasIndexSignature<T> extends true
  ? GetValidIndexSignature<T> | GetRecordPaths<RemoveIndexSignature<T>, DepthCarry>
  : GetRecordPaths<T, DepthCarry>

/**
 * Checks for the remaining depth leveL and stops further traversal if necessary.
 * Reduces the depth level by one if it traverse deeper.
 *
 * @typeParam T - Type to check
 * @typeParam DepthCarry - Tuple that is the size of the remaining depth level
 */
type TraversalGate<T, DepthCarry extends unknown[]> = TupleLength<DepthCarry> extends 0
  ? IsPrimitive<T> extends true // we only give the possibility to traversal further down, if the current type is not a primitive
    ? never
    : any
  : T extends T
  ? Traverse<T, MinusOne<DepthCarry>>
  : never

/**
 * Traverse the given type.
 * Removes problematic modifiers like readonly or undefined, null union types from it.
 *
 * @typeParam T - Type to traverse
 * @typeParam DepthCarry - Tuple that is the size of the remaining depth level
 */
type Traverse<T, DepthCarry extends unknown[]> = TraversalStep<
  Writeable<ExcludeNullUndefined<T>>,
  DepthCarry
>

/**
 * Simple Path return all paths in the given type and returns them until it reaches its max depth.
 *
 * @typeParam T - Type to traverse
 * @typeParam Depth - Max depth to traverse
 */
type SimplePath<T, Depth extends number> = Traverse<T, BuildTuple<Depth>>

/* -------------------------------------------------------------------------- */
/*                                    Path                                    */
/* -------------------------------------------------------------------------- */

/**
 * Similiar to SimplePath, Path return all paths in the given type and returns them until it reaches its max depth.
 *
 * The difference is that Path allows to specify an offset to start the traversal from. It will start the traversal
 * by the last parent in the given offset.
 *
 * a.b.c.d
 *     ^- return possible paths from here
 *
 * @typeParam T - Type to traverse
 * @typeParam Depth - Max depth to traverse
 * @typeParam Offset - Offset path
 */
type Path<
  T,
  Depth extends number = 3,
  Offset extends string | number = never
> = IsAny<Offset> extends true
  ? SimplePath<T, Depth>
  : IsNever<Offset> extends true
  ? SimplePath<T, Depth>
  : // eslint-disable-next-line @typescript-eslint/no-unused-vars
  Offset extends `${string}.${string}`
  ? BeforeLast<Offset, '.'> extends infer H
    ? H extends string | number
      ? PathValue<T, H, 40> extends infer V
        ? IsNever<V> extends true
          ? SimplePath<T, Depth>
          : `${H}.${SimplePath<V, Depth>}`
        : SimplePath<T, Depth>
      : never
    : never
  : Offset extends keyof T
  ? Offset | `${Offset}.${SimplePath<T[Offset], Depth>}`
  : SimplePath<T, Depth>

/**
 *
 * CicrcularConstraintPathHelper allows us to use Path as a cyclic constrainted for a generic parameter. e.g.
 * function foo<T, P extends CircularConstraintPathHelper<T, 3, [P]>>(...)
 *
 * This allows us to use the given path as the offset parameter for the path type.
 *
 * TODO: Find out why this works and if it may break in the future.
 *
 * @typeParam T - Type to traverse
 * @typeParam Depth - Max depth to traverse
 * @typeParam P - Should be a tuple with the path to traverse from as its only element, e.g. [P]
 */
type CircularConstraintPathHelper<
  T,
  Depth extends number,
  P extends unknown[]
> = P[number] extends infer PV
  ? PV extends string | number
    ? Path<T, Depth, PV>
    : never
  : SimplePath<T, Depth>

/* -------------------------------------------------------------------------- */
/*                                 Path Value                                 */
/* -------------------------------------------------------------------------- */

type ValueTraversalStep<T, P, DepthCarry extends unknown[]> = IsAny<T> extends true
  ? any
  : IsUnknown<T> extends true
  ? unknown
  : IsNullableOrUndefineable<T> extends true
  ? ValueTraversalGate<ExcludeNullUndefined<T>, P, DepthCarry> | undefined
  : IsTuple<T> extends true
  ? P extends `${infer H}.${infer R extends number}`
    ? ValueTraversalGate<TupleElement<T, H>, R, DepthCarry>
    : P extends `${infer K extends number}`
    ? TupleElement<T, K> | undefined
    : never
  : IsArray<T> extends true
  ? // eslint-disable-next-line @typescript-eslint/no-unused-vars
    P extends `${infer _H extends number}.${infer R}`
    ? ValueTraversalGate<GetArrayElement<T>, R, DepthCarry> | undefined
    : // eslint-disable-next-line @typescript-eslint/no-unused-vars
    P extends `${infer _K extends number}`
    ? GetArrayElement<T> | undefined
    : never
  : P extends `${infer H}.${infer R}`
  ? H extends keyof T
    ?
        | ValueTraversalGate<T[H], R, DepthCarry>
        | (HasIndexSignature<T> extends true ? undefined : never)
    : never
  : P extends keyof T
  ? T[P] | (HasIndexSignature<T> extends true ? undefined : never)
  : never

type ValueTraversalGate<T, P, DepthCarry extends unknown[]> = TupleLength<DepthCarry> extends 0
  ? unknown
  : T extends undefined | null
  ? undefined | TraverseValue<T, P, MinusOne<DepthCarry>>
  : TraverseValue<T, P, MinusOne<DepthCarry>>

type TraverseValue<T, P, DepthCarry extends unknown[]> = ValueTraversalStep<
  Writeable<ExcludeNullUndefined<T>>,
  P,
  DepthCarry
>

type PathValue<T, P, Depth extends number = 40> = TraverseValue<T, P, BuildTuple<Depth>>

type SearchableObject = Record<never, never> | unknown[]

/**
 * Retrives a value from an object by dot notation
 *
 * @param object - object to get value from
 * @param path - path to value
 *
 * @privateRemarks
 * The intersection between PathEntry<T, 10>  and string is necessary for TypeScript to successfully narrow down the type of P based on the user-provided path input.
 * Without the intersection, the path would just be of type PathEntry<T, 10> and PathValueEntry would be a union of all possible return types.
 * By using the intersection, TypeScript is forced to apply the PathEntry constraints and infer the type from the provided user input.
 */
function getByPath<T extends SearchableObject, P extends CircularConstraintPathHelper<T, 1, [P]>>(
  object: T,
  path: P & string // required so type gets narrowed down from its union of possible paths to its actual value. TODO: Find Ts-Issue or similiar why this is required.
): PathValue<T, P> {
  const pathArray = (path as string).split('.')

  return pathArray.reduce((accumulator: any, current) => accumulator?.[current], object) as any
}

/**
 * Sets a value in an object by dot notation
 * @param object - object to set value in
 * @param path - path to value
 * @param value - value to set
 *
 * @privateRemarks
 * The intersection between PathEntry<T, 10>  and string is necessary for TypeScript to successfully narrow down the type of P based on the user-provided path input.
 * Without the intersection, the path would just be of type PathEntry<T, 10> and PathValueEntry would be a union of all possible return types.
 * By using the intersection, TypeScript is forced to apply the PathEntry constraints and infer the type from the provided user input.
 */
function setByPath<
  T extends SearchableObject,
  P extends SimplePath<T, 10> & string,
  V extends PathValue<T, P>
>(object: T, path: P, value: V): void {
  const pathArray = (path as string).split('.')
  const lastKey = pathArray.pop()

  if (lastKey === undefined) {
    throw new Error('Path is empty')
  }

  const objectToSet = pathArray.reduce(
    (accumulator: any, current) => accumulator?.[current],
    object
  )

  if (objectToSet === undefined) {
    throw new Error('Path is invalid')
  }

  objectToSet[lastKey] = value
}

export type { Path, PathValue, SearchableObject }

export { getByPath, setByPath }
