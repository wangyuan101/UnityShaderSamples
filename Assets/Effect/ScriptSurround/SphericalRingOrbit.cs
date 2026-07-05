using Sirenix.OdinInspector;
using UnityEngine;
using UnityEngine.Serialization;

[ExecuteAlways]
public class SphericalRingOrbit : MonoBehaviour
{
    private const float MIN_POSITION_OFFSET_SQR = 0.0001f;
    private const float AXIS_LIMIT_TOLERANCE = 0.01f;
    private const float RADIUS_LIMIT_TOLERANCE = 0.001f;

    private bool runEnabled;

    [Header("Prefab")]
    [Tooltip("生成后沿轨道运动的预制体。")]
    [SerializeField] private GameObject prefab;

    [Header("Path")]
    [Tooltip("生成实例开始运动的位置，相对于当前 Transform。")]
    [SerializeField] private Vector3 startPosition = Vector3.right;
    [Tooltip("与起始位置一起用于计算初始轨道轴的位置，相对于当前 Transform。")]
    [SerializeField] private Vector3 endPosition = Vector3.back;

    [Header("Orbit")]
    [Tooltip("应用起始轴偏移前的基础轨道平面法线。")]
    [SerializeField] private Vector3 initialAxis = Vector3.up;
    [Tooltip("基础半径，运行时半径会在该值到该值加随机半径偏移之间变化。")]
    [SerializeField] [FormerlySerializedAs("radius")] [Min(0f)] private float maxStartRadius = 1f;
    [Tooltip("运行时半径摆动可增加的最大额外半径。")]
    [SerializeField] [Min(0f)] private float radiusRandomOffsetRange;
    [Tooltip("运行时半径偏移往复变化的速度。")]
    [SerializeField] [Min(0f)] private float radiusRandomChangeSpeed = 1f;
    [Tooltip("轨道角速度，单位为度/秒。")]
    [SerializeField] private float orbitSpeed = 90f;

    [Header("Axis Rotation")]
    [Tooltip("每秒添加到轨道轴偏移上的欧拉角速度。")]
    [SerializeField] private Vector3 axisRotationSpeed;
    [Tooltip("计算和动画过程中轨道轴允许的最大欧拉角绝对偏移。")]
    [SerializeField] private Vector3 maxAxisOffsetEuler = new Vector3(0f, 180f, 0f);

    [Header("Orientation")]
    [Tooltip("生成实例朝移动方向旋转时使用的平滑速度。")]
    [SerializeField] [Min(0f)] private float rotationSmoothSpeed = 20f;

    [Tooltip("用于朝向计算的移动方向平滑速度。")]
    [SerializeField] [Min(0f)] private float directionSmoothSpeed = 20f;

    [Header("Position Attraction")]
    [Tooltip("吸引生成实例靠近轨道目标位置时使用的加速度。")]
    [SerializeField] [Min(0f)] private float positionAttractionAcceleration = 30f;
    [Tooltip("作用在吸引速度上的阻尼，用于减少过冲。")]
    [SerializeField] [Min(0f)] private float positionAttractionDamping = 8f;

    [Header("Stop Follow")]
    [Tooltip("停止轨道后，按归一化时间采样并写入 IndexedFollowerChain.FollowDistance 的曲线。")]
    [SerializeField] private AnimationCurve stopFollowDistanceCurve = AnimationCurve.Linear(0f, 0.5f, 1f, 0f);
    [Tooltip("停止轨道后驱动 Follow Distance 曲线的持续时间，单位为秒。")]
    [SerializeField] [Min(0f)] private float stopFollowDistanceDuration = 1f;

    private GameObject spawnedInstance;
    private float orbitAngle;
    private float runtimeBaseRadius;
    private float runtimeRadius;
    private Vector3 runtimeAxisOffset;
    private Vector3 runtimeAxisDirection;
    private Vector3 previousRadialDirection;
    private bool hasPreviousRadialDirection;
    private bool hasAppliedRotation;
    private Vector3 previousInstancePosition;
    private bool hasPreviousInstancePosition;
    private Vector3 smoothedTravelDirection;
    private bool hasSmoothedTravelDirection;
    private float runtimeRadiusOffset;
    private float runtimeRadiusOffsetDirection = 1f;
    private Vector3 positionVelocity;
    private GameObject spawnedPrefab;
    private IndexedFollowerChain stopFollowerChain;
    private float stopFollowDistanceElapsed;
    private bool stopFollowDistanceActive;

    [Button("Start Orbit")]
    [HideIf(nameof(IsRunning))]
    public void StartOrbit()
    {
        ResetStopFollowDistance();

        if (!InitializeRuntimeState())
        {
            StopOrbit();
            return;
        }

        runEnabled = true;

        EnsureInstance();
        UpdateOrbitPosition(0f, true);
        ResetSpawnedFollowerChain();
    }

    public void StartOrbit(Vector3 worldStartPosition)
    {
        startPosition = transform.InverseTransformPoint(worldStartPosition);
        StartOrbit();
    }

    [Button("End Orbit")]
    [ShowIf(nameof(IsRunning))]
    public void EndOrbit()
    {
        StopOrbit();
    }

    public void EndOrbit(Vector3 worldEndPosition)
    {
        endPosition = transform.InverseTransformPoint(worldEndPosition);
        EndOrbit();
    }

    private void OnDisable()
    {
        runEnabled = false;
        ResetStopFollowDistance();
        DestroySpawnedInstance();
        ResetPreviewState();
        ResetAttractionState();
    }

    private void Update()
    {
        float deltaTime = GetDeltaTime();
        UpdateStopFollowDistance(deltaTime);

        if (!CanRunInCurrentContext())
        {
            return;
        }

        Tick(deltaTime);
    }

    private void Tick(float deltaTime)
    {
        if (!CanRunInCurrentContext())
        {
            return;
        }

        if (spawnedInstance == null)
        {
            EnsureInstance();
        }

        if (spawnedInstance == null)
        {
            return;
        }

        runtimeAxisOffset = AdvanceAxisOffset(runtimeAxisOffset, deltaTime);
        orbitAngle += orbitSpeed * deltaTime;
        UpdateRuntimeRadius(deltaTime);

        UpdateOrbitPosition(deltaTime);
    }

    private void OnValidate()
    {
        if (!CanRunInCurrentContext())
        {
            return;
        }

        if (!InitializeRuntimeState())
        {
            StopOrbit();
            return;
        }

        runtimeAxisDirection = SyncAxisDirection(runtimeAxisDirection);
        UpdateRuntimeRadius(Application.isPlaying ? Time.deltaTime : 0f);
        UpdateOrbitPosition(0f, true);
    }

    private void OnDestroy()
    {
        DestroySpawnedInstance();
    }

    private void DestroySpawnedInstance()
    {
        if (spawnedInstance == null)
        {
            return;
        }

        if (Application.isPlaying)
        {
            Destroy(spawnedInstance);
        }
        else
        {
            DestroyImmediate(spawnedInstance);
        }

        spawnedInstance = null;
        spawnedPrefab = null;
    }

    private void OnDrawGizmos()
    {
        Quaternion referenceRotation = transform.rotation;
        Vector3 axis;
        float gizmoRadius;
        Vector3 radialDirection;
        if (IsRunning())
        {
            axis = GetWorldAxis(referenceRotation, runtimeAxisOffset);
            gizmoRadius = runtimeRadius;
            radialDirection = GetContinuousRadialDirection(axis, referenceRotation);
        }
        else if (!TryResolveStartOrbit(out Vector3 startAxisOffset, out float startRadius, out Vector3 startRadialDirection, false))
        {
            DrawPathGizmos();
            return;
        }
        else
        {
            axis = GetWorldAxis(referenceRotation, startAxisOffset);
            gizmoRadius = startRadius;
            radialDirection = startRadialDirection;
        }

        float axisLength = Mathf.Max(gizmoRadius, 0.5f);
        float pivotSize = Mathf.Max(gizmoRadius * 0.05f, 0.03f);
        float targetSize = Mathf.Max(gizmoRadius * 0.08f, 0.05f);
        Vector3 currentRadialDirection = Quaternion.AngleAxis(orbitAngle, axis) * radialDirection;
        Vector3 orbitTargetPosition = transform.position + currentRadialDirection * gizmoRadius;
        Color previousColor = Gizmos.color;

        Gizmos.color = Color.cyan;
        Gizmos.DrawSphere(transform.position, pivotSize);
        Gizmos.DrawLine(transform.position - axis * axisLength, transform.position + axis * axisLength);

        Gizmos.color = Color.yellow;
        Gizmos.DrawLine(transform.position, orbitTargetPosition);
        Gizmos.DrawSphere(orbitTargetPosition, targetSize);
        DrawPathGizmos();

        Gizmos.color = previousColor;
    }

    private void EnsureInstance()
    {
        if (!CanRunInCurrentContext())
        {
            return;
        }

        if (spawnedInstance != null && spawnedPrefab == prefab)
        {
            return;
        }

        DestroySpawnedInstance();

        if (prefab == null)
        {
            return;
        }

        spawnedInstance = Instantiate(prefab);
        spawnedInstance.name = prefab.name;
        spawnedPrefab = prefab;

        if (!Application.isPlaying)
        {
            spawnedInstance.hideFlags = HideFlags.DontSave;
        }

        hasAppliedRotation = false;
        hasPreviousInstancePosition = false;
        hasSmoothedTravelDirection = false;
        ResetAttractionState();
    }

    private void ResetSpawnedFollowerChain()
    {
        if (spawnedInstance == null)
        {
            return;
        }

        if (spawnedInstance.TryGetComponent(out IndexedFollowerChain indexedFollowerChain))
        {
            indexedFollowerChain.ResetChainToCurrentPose();
        }
    }

    private bool InitializeRuntimeState()
    {
        if (!TryResolveStartOrbit(out runtimeAxisOffset, out float startRadius, out Vector3 startRadialDirection, true))
        {
            return false;
        }

        orbitAngle = 0f;
        runtimeBaseRadius = Mathf.Max(0f, maxStartRadius);
        runtimeAxisDirection = GetInitialAxisDirection();
        ResetRuntimeRadius(startRadius);
        ResetPreviewState();
        previousRadialDirection = startRadialDirection;
        hasPreviousRadialDirection = true;
        return true;
    }

    private void ResetPreviewState()
    {
        hasAppliedRotation = false;
        hasPreviousRadialDirection = false;
        hasPreviousInstancePosition = false;
        hasSmoothedTravelDirection = false;
    }

    private bool IsRunning()
    {
        return runEnabled;
    }

    private void StopOrbit()
    {
        MoveSpawnedInstanceToEndPosition();
        StartStopFollowDistance();
        runEnabled = false;
        ResetPreviewState();
        ResetAttractionState();
    }

    private void MoveSpawnedInstanceToEndPosition()
    {
        if (spawnedInstance == null)
        {
            return;
        }

        Vector3 endWorldPosition = GetWorldEndPosition();
        ApplyEndPositionRotation(endWorldPosition);

        spawnedInstance.SetActive(true);
        spawnedInstance.transform.position = endWorldPosition;
    }

    private void ApplyEndPositionRotation(Vector3 endWorldPosition)
    {
        Vector3 toEndPosition = endWorldPosition - spawnedInstance.transform.position;
        if (toEndPosition.sqrMagnitude < 0.0001f)
        {
            return;
        }

        spawnedInstance.transform.rotation = Quaternion.LookRotation(toEndPosition.normalized, GetSafeEndRotationUp(toEndPosition));
    }

    private Vector3 GetSafeEndRotationUp(Vector3 forward)
    {
        Vector3 up = spawnedInstance.transform.up;
        if (up.sqrMagnitude < 0.0001f)
        {
            up = Vector3.up;
        }

        Vector3 safeForward = forward.normalized;
        Vector3 safeUp = up.normalized;
        if (Mathf.Abs(Vector3.Dot(safeForward, safeUp)) > 0.999f)
        {
            safeUp = Vector3.up;
        }

        if (Mathf.Abs(Vector3.Dot(safeForward, safeUp)) > 0.999f)
        {
            safeUp = Vector3.right;
        }

        return safeUp;
    }

    private void StartStopFollowDistance()
    {
        ResetStopFollowDistance();

        if (spawnedInstance == null
            || !spawnedInstance.TryGetComponent(out stopFollowerChain)
            || stopFollowDistanceCurve == null)
        {
            return;
        }

        stopFollowDistanceElapsed = 0f;
        stopFollowDistanceActive = true;

        if (stopFollowDistanceDuration <= 0f)
        {
            ApplyStopFollowDistance(1f);
            ResetStopFollowDistance();
            return;
        }

        ApplyStopFollowDistance(0f);
    }

    private void UpdateStopFollowDistance(float deltaTime)
    {
        if (!stopFollowDistanceActive || stopFollowerChain == null)
        {
            return;
        }

        if (stopFollowDistanceDuration <= 0f)
        {
            ApplyStopFollowDistance(1f);
            ResetStopFollowDistance();
            return;
        }

        stopFollowDistanceElapsed += Mathf.Max(0f, deltaTime);
        float normalizedTime = Mathf.Clamp01(stopFollowDistanceElapsed / stopFollowDistanceDuration);
        ApplyStopFollowDistance(normalizedTime);

        if (normalizedTime >= 1f)
        {
            ResetStopFollowDistance();
        }
    }

    private void ApplyStopFollowDistance(float normalizedTime)
    {
        if (stopFollowerChain == null || stopFollowDistanceCurve == null)
        {
            return;
        }

        stopFollowerChain.FollowDistance = stopFollowDistanceCurve.Evaluate(Mathf.Clamp01(normalizedTime));
    }

    private void ResetStopFollowDistance()
    {
        stopFollowerChain = null;
        stopFollowDistanceElapsed = 0f;
        stopFollowDistanceActive = false;
    }

    private bool TryResolveStartOrbit(out Vector3 axisOffset, out float startRadius, out Vector3 startRadialDirection, bool logError)
    {
        axisOffset = Vector3.zero;
        startRadius = 0f;
        startRadialDirection = Vector3.zero;

        Vector3 pivot = transform.position;
        Vector3 startOffset = GetWorldStartPosition() - pivot;
        Vector3 endOffset = GetWorldEndPosition() - pivot;

        if (startOffset.sqrMagnitude < MIN_POSITION_OFFSET_SQR)
        {
            if (logError)
            {
                Debug.LogError($"{nameof(SphericalRingOrbit)} startPosition is too close to transform position.", this);
            }
            return false;
        }

        startRadius = startOffset.magnitude;
        float minRuntimeRadius = Mathf.Max(0f, maxStartRadius);
        float maxRuntimeRadius = minRuntimeRadius + Mathf.Max(0f, radiusRandomOffsetRange);
        if (startRadius < minRuntimeRadius - RADIUS_LIMIT_TOLERANCE
            || startRadius > maxRuntimeRadius + RADIUS_LIMIT_TOLERANCE)
        {
            if (logError)
            {
                Debug.LogError($"{nameof(SphericalRingOrbit)} start radius {startRadius:F3} is outside runtime radius range {minRuntimeRadius:F3} - {maxRuntimeRadius:F3}.", this);
            }
            return false;
        }

        Quaternion referenceRotation = transform.rotation;
        Vector3 safeInitialAxis = initialAxis.sqrMagnitude > 0.0001f ? initialAxis.normalized : Vector3.up;
        Vector3 axis = ResolveStartOrbitAxis(startOffset, endOffset, referenceRotation, safeInitialAxis);
        Vector3 localAxis = Quaternion.Inverse(referenceRotation) * axis;
        axisOffset = NormalizeEuler(Quaternion.FromToRotation(safeInitialAxis, localAxis).eulerAngles);

        if (!IsAxisOffsetInLimit(axisOffset))
        {
            if (logError)
            {
                Debug.LogError($"{nameof(SphericalRingOrbit)} resolved start axis {axisOffset} exceeds maxAxisOffsetEuler {maxAxisOffsetEuler}.", this);
            }
            return false;
        }

        startRadialDirection = startOffset.normalized;
        return true;
    }

    private Vector3 ResolveStartOrbitAxis(Vector3 startOffset, Vector3 endOffset, Quaternion referenceRotation, Vector3 safeInitialAxis)
    {
        Vector3 axis = Vector3.Cross(startOffset, endOffset);
        if (axis.sqrMagnitude >= MIN_POSITION_OFFSET_SQR)
        {
            axis.Normalize();
            if (orbitSpeed < 0f)
            {
                axis = -axis;
            }

            return axis;
        }

        Vector3 preferredAxis = referenceRotation * safeInitialAxis;
        axis = Vector3.ProjectOnPlane(preferredAxis, startOffset);
        if (axis.sqrMagnitude < MIN_POSITION_OFFSET_SQR)
        {
            axis = Vector3.Cross(startOffset, referenceRotation * Vector3.right);
        }

        if (axis.sqrMagnitude < MIN_POSITION_OFFSET_SQR)
        {
            axis = Vector3.Cross(startOffset, referenceRotation * Vector3.forward);
        }

        if (axis.sqrMagnitude < MIN_POSITION_OFFSET_SQR)
        {
            axis = Vector3.Cross(startOffset, Vector3.up);
        }

        if (axis.sqrMagnitude < MIN_POSITION_OFFSET_SQR)
        {
            axis = Vector3.Cross(startOffset, Vector3.right);
        }

        return axis.normalized;
    }

    private bool CanRunInCurrentContext()
    {
        return IsRunning();
    }

    private void ResetRuntimeRadius(float startRadius)
    {
        maxStartRadius = Mathf.Max(0f, maxStartRadius);
        runtimeBaseRadius = Mathf.Max(0f, runtimeBaseRadius);
        radiusRandomOffsetRange = Mathf.Max(0f, radiusRandomOffsetRange);
        radiusRandomChangeSpeed = Mathf.Max(0f, radiusRandomChangeSpeed);

        runtimeRadiusOffset = Mathf.Clamp(startRadius - runtimeBaseRadius, 0f, radiusRandomOffsetRange);
        runtimeRadiusOffsetDirection = runtimeRadiusOffset >= radiusRandomOffsetRange - RADIUS_LIMIT_TOLERANCE ? -1f : 1f;
        runtimeRadius = Mathf.Max(0f, runtimeBaseRadius + runtimeRadiusOffset);
    }

    private void UpdateRuntimeRadius(float deltaTime)
    {
        maxStartRadius = Mathf.Max(0f, maxStartRadius);
        runtimeBaseRadius = Mathf.Max(0f, runtimeBaseRadius);
        radiusRandomOffsetRange = Mathf.Max(0f, radiusRandomOffsetRange);
        radiusRandomChangeSpeed = Mathf.Max(0f, radiusRandomChangeSpeed);

        if (runtimeRadiusOffsetDirection == 0f)
        {
            runtimeRadiusOffsetDirection = 1f;
        }

        float targetRadiusOffset = runtimeRadiusOffsetDirection > 0f ? radiusRandomOffsetRange : 0f;
        float maxDelta = radiusRandomChangeSpeed * deltaTime;
        runtimeRadiusOffset = Mathf.MoveTowards(runtimeRadiusOffset, targetRadiusOffset, maxDelta);

        if (radiusRandomOffsetRange > 0f && Mathf.Approximately(runtimeRadiusOffset, targetRadiusOffset))
        {
            runtimeRadiusOffsetDirection *= -1f;
        }

        runtimeRadius = Mathf.Max(0f, runtimeBaseRadius + runtimeRadiusOffset);
    }

    private void UpdateOrbitPosition(float deltaTime, bool snapToOrbitTarget = false)
    {
        if (spawnedInstance == null)
        {
            return;
        }

        spawnedInstance.SetActive(true);

        Quaternion referenceRotation = transform.rotation;
        Vector3 axis = GetWorldAxis(referenceRotation, runtimeAxisOffset);
        Vector3 radialDirection = GetContinuousRadialDirection(axis, referenceRotation);
        Vector3 currentRadialDirection = Quaternion.AngleAxis(orbitAngle, axis) * radialDirection;
        Vector3 orbitOffset = currentRadialDirection * runtimeRadius;
        Vector3 orbitTargetPosition = transform.position + orbitOffset;
        bool shouldSnapToOrbitTarget = snapToOrbitTarget || !hasPreviousInstancePosition;
        Vector3 instancePosition = shouldSnapToOrbitTarget
            ? orbitTargetPosition
            : ResolveAttractedPosition(orbitTargetPosition, deltaTime);

        if (shouldSnapToOrbitTarget)
        {
            positionVelocity = Vector3.zero;
        }

        Vector3 tangentDirection = GetSmoothedTravelDirection(instancePosition, axis, currentRadialDirection, deltaTime);

        spawnedInstance.transform.position = instancePosition;
        ApplyOrbitRotation(tangentDirection, axis, deltaTime);
        previousInstancePosition = instancePosition;
        hasPreviousInstancePosition = true;
        previousRadialDirection = radialDirection;
        hasPreviousRadialDirection = true;
    }

    private Vector3 ResolveAttractedPosition(Vector3 orbitTargetPosition, float deltaTime)
    {
        Vector3 instancePosition = spawnedInstance.transform.position;

        if (deltaTime <= 0f)
        {
            return instancePosition;
        }

        Vector3 toTarget = orbitTargetPosition - instancePosition;
        positionVelocity += toTarget * positionAttractionAcceleration * deltaTime;

        if (positionAttractionDamping > 0f)
        {
            positionVelocity *= Mathf.Exp(-positionAttractionDamping * deltaTime);
        }

        return instancePosition + positionVelocity * deltaTime;
    }

    private void ResetAttractionState()
    {
        positionVelocity = Vector3.zero;
    }

    private float GetDeltaTime()
    {
        return Time.deltaTime;
    }

    private Vector3 GetWorldAxis(Quaternion referenceRotation, Vector3 axisOffset)
    {
        Vector3 safeAxis = initialAxis.sqrMagnitude > 0.0001f ? initialAxis.normalized : Vector3.up;
        Quaternion axisRotation = referenceRotation * Quaternion.Euler(ClampAxisOffset(axisOffset));
        return (axisRotation * safeAxis).normalized;
    }

    private void DrawPathGizmos()
    {
        Vector3 worldStartPosition = GetWorldStartPosition();
        Vector3 worldEndPosition = GetWorldEndPosition();
        float pointSize = Mathf.Max((worldStartPosition - transform.position).magnitude * 0.05f, 0.05f);

        Gizmos.color = Color.green;
        Gizmos.DrawSphere(worldStartPosition, pointSize);

        Gizmos.color = Color.red;
        Gizmos.DrawSphere(worldEndPosition, pointSize);
        Gizmos.DrawLine(worldStartPosition, worldEndPosition);
    }

    private Vector3 GetWorldStartPosition()
    {
        return transform.TransformPoint(startPosition);
    }

    private Vector3 GetWorldEndPosition()
    {
        return transform.TransformPoint(endPosition);
    }

    private static Vector3 BuildRadialDirection(Vector3 axis, Quaternion referenceRotation)
    {
        Vector3 radial = Vector3.ProjectOnPlane(referenceRotation * Vector3.right, axis);
        if (radial.sqrMagnitude < 0.0001f)
        {
            radial = Vector3.ProjectOnPlane(referenceRotation * Vector3.forward, axis);
        }

        if (radial.sqrMagnitude < 0.0001f)
        {
            radial = Vector3.ProjectOnPlane(Vector3.right, axis);
        }

        if (radial.sqrMagnitude < 0.0001f)
        {
            radial = Vector3.ProjectOnPlane(Vector3.forward, axis);
        }

        return radial.normalized;
    }

    private Vector3 GetContinuousRadialDirection(Vector3 axis, Quaternion referenceRotation)
    {
        if (hasPreviousRadialDirection)
        {
            Vector3 radial = Vector3.ProjectOnPlane(previousRadialDirection, axis);
            if (radial.sqrMagnitude > 0.0001f)
            {
                return radial.normalized;
            }
        }

        return BuildRadialDirection(axis, referenceRotation);
    }

    private void ApplyOrbitRotation(Vector3 tangentDirection, Vector3 axis, float deltaTime)
    {
        if (tangentDirection.sqrMagnitude < 0.0001f)
        {
            return;
        }

        Vector3 forward = tangentDirection.normalized;
        Vector3 up = Vector3.ProjectOnPlane(axis, forward);
        if (up.sqrMagnitude < 0.0001f)
        {
            up = Vector3.ProjectOnPlane(Vector3.up, forward);
        }

        if (up.sqrMagnitude < 0.0001f)
        {
            up = Vector3.ProjectOnPlane(Vector3.right, forward);
        }

        Quaternion targetRotation = Quaternion.LookRotation(forward, up.normalized);

        if (!hasAppliedRotation)
        {
            spawnedInstance.transform.rotation = targetRotation;
        }
        else
        {
            float rotationT = 1f - Mathf.Exp(-rotationSmoothSpeed * deltaTime);
            spawnedInstance.transform.rotation = Quaternion.Slerp(
                spawnedInstance.transform.rotation,
                targetRotation,
                rotationT);
        }

        hasAppliedRotation = true;
    }

    private Vector3 GetSmoothedTravelDirection(Vector3 instancePosition, Vector3 axis, Vector3 radialDirection, float deltaTime)
    {
        Vector3 travelDirection = hasPreviousInstancePosition
            ? instancePosition - previousInstancePosition
            : Vector3.zero;

        if (travelDirection.sqrMagnitude < 0.0001f)
        {
            travelDirection = BuildOrbitTangent(axis, radialDirection, orbitSpeed);
        }

        if (travelDirection.sqrMagnitude < 0.0001f)
        {
            return Vector3.zero;
        }

        travelDirection.Normalize();

        if (!hasSmoothedTravelDirection)
        {
            smoothedTravelDirection = travelDirection;
            hasSmoothedTravelDirection = true;
            return smoothedTravelDirection;
        }

        float directionT = 1f - Mathf.Exp(-directionSmoothSpeed * deltaTime);
        smoothedTravelDirection = Vector3.Slerp(smoothedTravelDirection, travelDirection, directionT).normalized;
        return smoothedTravelDirection;
    }

    private static Vector3 BuildOrbitTangent(Vector3 axis, Vector3 radialDirection, float speed)
    {
        if (radialDirection.sqrMagnitude < 0.0001f || Mathf.Approximately(speed, 0f))
        {
            return Vector3.zero;
        }

        Vector3 safeRadialDirection = radialDirection.normalized;
        float signedStep = speed > 0f ? 1f : -1f;
        Vector3 nextRadialDirection = Quaternion.AngleAxis(signedStep, axis) * safeRadialDirection;
        return nextRadialDirection - safeRadialDirection;
    }

    private Vector3 ClampAxisOffset(Vector3 value)
    {
        return new Vector3(
            ClampSigned(value.x, maxAxisOffsetEuler.x),
            ClampSigned(value.y, maxAxisOffsetEuler.y),
            ClampSigned(value.z, maxAxisOffsetEuler.z));
    }

    private bool IsAxisOffsetInLimit(Vector3 value)
    {
        return IsSignedValueInLimit(value.x, maxAxisOffsetEuler.x)
               && IsSignedValueInLimit(value.y, maxAxisOffsetEuler.y)
               && IsSignedValueInLimit(value.z, maxAxisOffsetEuler.z);
    }

    private static bool IsSignedValueInLimit(float value, float limit)
    {
        return Mathf.Abs(value) <= Mathf.Abs(limit) + AXIS_LIMIT_TOLERANCE;
    }

    private static Vector3 NormalizeEuler(Vector3 value)
    {
        return new Vector3(
            NormalizeEulerAngle(value.x),
            NormalizeEulerAngle(value.y),
            NormalizeEulerAngle(value.z));
    }

    private static float NormalizeEulerAngle(float value)
    {
        value = Mathf.Repeat(value + 180f, 360f) - 180f;
        return Mathf.Approximately(value, -180f) ? 180f : value;
    }

    private Vector3 AdvanceAxisOffset(Vector3 currentOffset, float deltaTime)
    {
        return new Vector3(
            AdvanceAxisComponent(currentOffset.x, Mathf.Abs(axisRotationSpeed.x), ref runtimeAxisDirection.x, maxAxisOffsetEuler.x, deltaTime),
            AdvanceAxisComponent(currentOffset.y, Mathf.Abs(axisRotationSpeed.y), ref runtimeAxisDirection.y, maxAxisOffsetEuler.y, deltaTime),
            AdvanceAxisComponent(currentOffset.z, Mathf.Abs(axisRotationSpeed.z), ref runtimeAxisDirection.z, maxAxisOffsetEuler.z, deltaTime));
    }

    private Vector3 GetInitialAxisDirection()
    {
        return new Vector3(
            GetAxisDirection(axisRotationSpeed.x),
            GetAxisDirection(axisRotationSpeed.y),
            GetAxisDirection(axisRotationSpeed.z));
    }

    private Vector3 SyncAxisDirection(Vector3 currentDirection)
    {
        return new Vector3(
            ResolveAxisDirection(currentDirection.x, axisRotationSpeed.x),
            ResolveAxisDirection(currentDirection.y, axisRotationSpeed.y),
            ResolveAxisDirection(currentDirection.z, axisRotationSpeed.z));
    }

    private static float AdvanceAxisComponent(float currentOffset, float speed, ref float direction, float limit, float deltaTime)
    {
        float safeLimit = Mathf.Abs(limit);
        if (safeLimit <= 0.0001f || speed <= 0.0001f)
        {
            direction = speed <= 0.0001f ? 0f : direction;
            return ClampSigned(currentOffset, safeLimit);
        }

        if (Mathf.Approximately(direction, 0f))
        {
            direction = 1f;
        }

        float nextOffset = currentOffset + speed * direction * deltaTime;
        while (nextOffset > safeLimit || nextOffset < -safeLimit)
        {
            if (nextOffset > safeLimit)
            {
                nextOffset = safeLimit - (nextOffset - safeLimit);
                direction = -1f;
            }
            else
            {
                nextOffset = -safeLimit + (-safeLimit - nextOffset);
                direction = 1f;
            }
        }

        return nextOffset;
    }

    private static float ClampSigned(float value, float limit)
    {
        float safeLimit = Mathf.Abs(limit);
        return Mathf.Clamp(value, -safeLimit, safeLimit);
    }

    private static float GetAxisDirection(float speed)
    {
        if (speed > 0f)
        {
            return 1f;
        }

        if (speed < 0f)
        {
            return -1f;
        }

        return 0f;
    }

    private static float ResolveAxisDirection(float currentDirection, float speed)
    {
        if (Mathf.Abs(speed) <= 0.0001f)
        {
            return 0f;
        }

        if (Mathf.Approximately(currentDirection, 0f))
        {
            return GetAxisDirection(speed);
        }

        return currentDirection;
    }
}
