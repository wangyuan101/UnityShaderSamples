using Sirenix.OdinInspector;
using UnityEngine;
using UnityEngine.Serialization;

[ExecuteAlways]
[DisallowMultipleComponent]
public class IndexedFollowerChain : MonoBehaviour
{
    [SerializeField] [UnityEngine.HideInInspector] private bool editorRunEnabled;

    [Header("Spawn")]
    // 要实例化的链条节点预制体。
    [Tooltip("要实例化的链条节点预制体。")]
    [SerializeField] private GameObject prefab;
    // 链条节点总数量，第一个节点索引固定为 0。
    [Tooltip("链条节点总数量，第一个节点索引固定为 0。")]
    [SerializeField] [Min(1)] private int instanceCount = 8;

    [Header("Sticky Follow")]
    // 相邻节点保持静止的固定距离，超过后才开始产生跟随加速度。
    [Tooltip("相邻节点保持静止的固定距离，超过后才开始产生跟随加速度。")]
    [FormerlySerializedAs("minDistance")]
    [SerializeField] [Min(0f)] private float followDistance = 0.5f;
    // 超出固定距离后的加速度强度，超出的距离越大，加速度越大。
    [Tooltip("超出固定距离后的加速度强度，超出的距离越大，加速度越大。")]
    [SerializeField] [Min(0f)] private float accelerationStrength = 40f;
    // 沿轨迹速度阻尼，数值越大越粘，越不容易震荡。
    [Tooltip("沿轨迹速度阻尼，数值越大越粘，越不容易震荡。")]
    [SerializeField] [Min(0f)] private float damping = 10f;

    [Header("Visual")]
    // 按索引归一化采样的缩放曲线，索引 0 采样 time 0，最大索引采样 time 1。
    [Tooltip("按索引归一化采样的缩放曲线，索引 0 采样 time 0，最大索引采样 time 1。")]
    [SerializeField] private AnimationCurve scaleCurve = AnimationCurve.Linear(0f, 1f, 1f, 1f);
    // 可见实例沿当前对象上方向上下偏移的最大距离。
    [Tooltip("可见实例沿当前对象上方向上下偏移的最大距离。")]
    [SerializeField] [Min(0f)] private float verticalOffsetDistance;
    // 上下偏移循环速度，单位为次每秒。
    [Tooltip("上下偏移循环速度，单位为次每秒。")]
    [SerializeField] [Min(0f)] private float verticalOffsetSpeed = 1f;
    // 可见实例沿当前对象右方向左右偏移的最大距离。
    [Tooltip("可见实例沿当前对象右方向左右偏移的最大距离。")]
    [SerializeField] [Min(0f)] private float horizontalOffsetDistance;
    // 左右偏移循环速度，单位为次每秒。
    [Tooltip("左右偏移循环速度，单位为次每秒。")]
    [SerializeField] [Min(0f)] private float horizontalOffsetSpeed = 1f;

    [Header("Sampling")]
    // 轨迹采样缓存数量，不是最大跟随距离；头节点移动很快或链条很长时需要增大。
    [Tooltip("轨迹采样缓存数量，不是最大跟随距离；头节点移动很快或链条很长时需要增大。")]
    [SerializeField] [Min(32)] private int historyCapacity = 512;

    // 运行时生成出来的所有运动根节点，索引 0 为头节点。
    private Transform[] spawnedTransforms;
    // 每个实例的第一个子物体，缩放和上下偏移只作用在这里，避免污染实例根节点。
    private Transform[] visualTransforms;
    // 第一个子物体的原始本地位置。
    private Vector3[] visualBaseLocalPositions;
    // 第一个子物体的原始本地缩放。
    private Vector3[] visualBaseLocalScales;
    // 每个节点当前位于整条轨迹上的累计距离。
    private float[] pathDistances;
    // 每个节点沿轨迹方向的当前速度。
    private float[] pathVelocities;
    // 每个可见实例上下偏移的随机相位，避免所有实例同步上下浮动。
    private float[] verticalOffsetPhases;
    // 每个可见实例左右偏移的随机相位，避免所有实例同步左右浮动。
    private float[] horizontalOffsetPhases;
    // 轨迹采样点的位置缓存。
    private Vector3[] historyPositions;
    // 轨迹采样点的旋转缓存。
    private Quaternion[] historyRotations;
    // 轨迹采样点对应的累计路径长度缓存。
    private float[] historyDistances;
    // 环形缓冲区中最旧采样点的起始索引。
    private int historyStart;
    // 当前有效采样点数量。
    private int historyCount;
    // 头节点累计走过的总路径长度。
    private float totalPathDistance;
    private GameObject spawnedPrefab;

    public float FollowDistance
    {
        get { return followDistance; }
        set { followDistance = Mathf.Max(0f, value); }
    }

    public void ResetChainToCurrentPose()
    {
        if (!HasValidChain())
        {
            RebuildChain();
            return;
        }

        ResetChainState();
    }

    [Button("开启Editor运行逻辑")]
    [HideInPlayMode]
    [HideIf(nameof(editorRunEnabled))]
    private void EnableEditorRun()
    {
        editorRunEnabled = true;
        RebuildChain();
    }

    [Button("关闭Editor运行逻辑")]
    [HideInPlayMode]
    [ShowIf(nameof(editorRunEnabled))]
    private void DisableEditorRun()
    {
        editorRunEnabled = false;
        DestroyChainInstances();
    }

    private void Awake()
    {
        if (!CanRunInCurrentContext())
        {
            return;
        }

        BuildChain();
        ResetChainState();
    }

    private void OnEnable()
    {
        if (!CanRunInCurrentContext())
        {
            return;
        }

        if (!HasValidChain())
        {
            RebuildChain();
        }

        ResetChainState();
    }

    private void OnDisable()
    {
        DestroyChainInstances();
    }

    private void LateUpdate()
    {
        if (!CanRunInCurrentContext())
        {
            return;
        }

        Tick(GetDeltaTime(), GetElapsedTime());
    }

    private void Tick(float deltaTime, float elapsedTime)
    {
        if (!CanRunInCurrentContext())
        {
            return;
        }

        if (!HasValidChain())
        {
            RebuildChain();
        }

        if (!HasValidChain())
        {
            return;
        }

        CaptureHeadPose(deltaTime);
        UpdateFollowers(deltaTime);
        UpdateVisualTransforms(elapsedTime);
    }

    private void OnValidate()
    {
        if (!CanRunInCurrentContext())
        {
            return;
        }

        if (HasValidChain())
        {
            EnsureHistoryCapacity();
            UpdateVisualTransforms(GetElapsedTime());
        }
    }

    private bool CanRunInCurrentContext()
    {
        return Application.isPlaying || editorRunEnabled;
    }

    private float GetDeltaTime()
    {
        return Time.deltaTime;
    }

    private float GetElapsedTime()
    {
        return Time.time;
    }

    private bool HasValidChain()
    {
        return prefab != null
            && spawnedPrefab == prefab
            && spawnedTransforms != null
            && visualTransforms != null
            && visualBaseLocalPositions != null
            && visualBaseLocalScales != null
            && pathDistances != null
            && pathVelocities != null
            && verticalOffsetPhases != null
            && horizontalOffsetPhases != null
            && spawnedTransforms.Length == instanceCount
            && visualTransforms.Length == instanceCount
            && visualBaseLocalPositions.Length == instanceCount
            && visualBaseLocalScales.Length == instanceCount
            && pathDistances.Length == instanceCount
            && pathVelocities.Length == instanceCount
            && verticalOffsetPhases.Length == instanceCount
            && horizontalOffsetPhases.Length == instanceCount
            && spawnedTransforms[0] != null;
    }

    private void BuildChain()
    {
        if (prefab == null)
        {
            return;
        }

        spawnedTransforms = new Transform[instanceCount];
        visualTransforms = new Transform[instanceCount];
        visualBaseLocalPositions = new Vector3[instanceCount];
        visualBaseLocalScales = new Vector3[instanceCount];
        pathDistances = new float[instanceCount];
        pathVelocities = new float[instanceCount];
        verticalOffsetPhases = new float[instanceCount];
        horizontalOffsetPhases = new float[instanceCount];

        for (int index = 0; index < instanceCount; index++)
        {
            GameObject instance = Instantiate(prefab, transform.position, transform.rotation, transform);
            instance.name = prefab.name + "_" + index;
            if (!Application.isPlaying)
            {
                instance.hideFlags = HideFlags.DontSave;
            }

            spawnedTransforms[index] = instance.transform;
            CacheVisualTransform(index, instance.transform);
        }

        spawnedPrefab = prefab;
    }

    private void OnDestroy()
    {
        DestroyChainInstances();
    }

    private void RebuildChain()
    {
        DestroyChainInstances();
        BuildChain();
        ResetChainState();
    }

    private void DestroyChainInstances()
    {
        if (spawnedTransforms == null)
        {
            spawnedPrefab = null;
            return;
        }

        for (int index = 0; index < spawnedTransforms.Length; index++)
        {
            if (spawnedTransforms[index] == null)
            {
                continue;
            }

            GameObject instance = spawnedTransforms[index].gameObject;
            if (Application.isPlaying)
            {
                Destroy(instance);
            }
            else
            {
                DestroyImmediate(instance);
            }
        }

        spawnedTransforms = null;
        visualTransforms = null;
        visualBaseLocalPositions = null;
        visualBaseLocalScales = null;
        pathDistances = null;
        pathVelocities = null;
        verticalOffsetPhases = null;
        horizontalOffsetPhases = null;
        historyPositions = null;
        historyRotations = null;
        historyDistances = null;
        historyStart = 0;
        historyCount = 0;
        totalPathDistance = 0f;
        spawnedPrefab = null;
    }

    private void ResetChainState()
    {
        if (!HasValidChain())
        {
            return;
        }

        Vector3 startPosition = transform.position;
        Quaternion startRotation = transform.rotation;
        EnsureHistoryCapacity();

        for (int index = 0; index < spawnedTransforms.Length; index++)
        {
            spawnedTransforms[index].SetPositionAndRotation(startPosition, startRotation);
            ApplyVisualTransform(index, GetElapsedTime());
            pathDistances[index] = 0f;
            pathVelocities[index] = 0f;
        }

        historyStart = 0;
        historyCount = 1;
        totalPathDistance = 0f;

        historyPositions[0] = startPosition;
        historyRotations[0] = startRotation;
        historyDistances[0] = 0f;
    }

    private void EnsureHistoryCapacity()
    {
        int requiredCapacity = Mathf.Max(32, historyCapacity);

        if (historyPositions != null && historyPositions.Length == requiredCapacity)
        {
            return;
        }

        Vector3[] oldPositions = historyPositions;
        Quaternion[] oldRotations = historyRotations;
        float[] oldDistances = historyDistances;
        int oldStart = historyStart;
        int oldCount = historyCount;
        int oldCapacity = oldPositions != null ? oldPositions.Length : 0;

        historyPositions = new Vector3[requiredCapacity];
        historyRotations = new Quaternion[requiredCapacity];
        historyDistances = new float[requiredCapacity];

        if (oldPositions == null
            || oldRotations == null
            || oldDistances == null
            || oldCapacity == 0
            || oldCount == 0)
        {
            historyStart = 0;
            historyCount = 0;
            return;
        }

        int copiedCount = Mathf.Min(oldCount, requiredCapacity);
        int firstOldLogicalIndex = oldCount - copiedCount;

        for (int index = 0; index < copiedCount; index++)
        {
            int oldIndex = (oldStart + firstOldLogicalIndex + index) % oldCapacity;
            historyPositions[index] = oldPositions[oldIndex];
            historyRotations[index] = oldRotations[oldIndex];
            historyDistances[index] = oldDistances[oldIndex];
        }

        historyStart = 0;
        historyCount = copiedCount;
    }

    private void CaptureHeadPose(float deltaTime)
    {
        Transform head = spawnedTransforms[0];
        Vector3 currentPosition = head.position;
        Quaternion currentRotation = head.rotation;

        if (historyCount == 0)
        {
            AddHistorySample(currentPosition, currentRotation, 0f);
            totalPathDistance = 0f;
            pathDistances[0] = 0f;
            pathVelocities[0] = 0f;
            return;
        }

        int latestIndex = GetBufferIndex(historyCount - 1);
        Vector3 previousPosition = historyPositions[latestIndex];
        float previousDistance = historyDistances[latestIndex];
        float stepDistance = Vector3.Distance(previousPosition, currentPosition);

        if (stepDistance <= 0.0001f)
        {
            historyRotations[latestIndex] = currentRotation;
            pathDistances[0] = totalPathDistance;
            pathVelocities[0] = 0f;
            return;
        }

        totalPathDistance = previousDistance + stepDistance;
        pathDistances[0] = totalPathDistance;
        pathVelocities[0] = stepDistance / Mathf.Max(deltaTime, 0.0001f);

        AddHistorySample(currentPosition, currentRotation, totalPathDistance);
    }

    private void UpdateFollowers(float deltaTime)
    {
        if (deltaTime <= 0f)
        {
            return;
        }

        int sampleUpperBound = historyCount - 1;
        float dampingFactor = Mathf.Exp(-damping * deltaTime);

        for (int index = 1; index < spawnedTransforms.Length; index++)
        {
            float leaderDistance = pathDistances[index - 1];
            float followerDistance = pathDistances[index];
            float gap = leaderDistance - followerDistance;
            float stretch = Mathf.Max(0f, gap - followDistance);
            float velocity = pathVelocities[index];
            float targetDistance = Mathf.Max(0f, leaderDistance - followDistance);

            if (targetDistance <= followerDistance)
            {
                pathVelocities[index] = 0f;
                ApplySampledPose(index, followerDistance, ref sampleUpperBound);
                continue;
            }

            if (stretch > 0f)
            {
                velocity += stretch * accelerationStrength * deltaTime;
            }

            velocity *= dampingFactor;

            float nextDistance = followerDistance + velocity * deltaTime;

            if (nextDistance >= targetDistance)
            {
                nextDistance = targetDistance;
                velocity = 0f;
            }

            if (nextDistance < followerDistance && stretch <= 0f)
            {
                nextDistance = followerDistance;
                velocity = 0f;
            }

            pathDistances[index] = Mathf.Max(0f, nextDistance);
            pathVelocities[index] = velocity;

            ApplySampledPose(index, pathDistances[index], ref sampleUpperBound);
        }
    }

    private void ApplySampledPose(int index, float distance, ref int sampleUpperBound)
    {
        Vector3 sampledPosition;
        Quaternion sampledRotation;
        SamplePose(distance, ref sampleUpperBound, out sampledPosition, out sampledRotation);
        spawnedTransforms[index].SetPositionAndRotation(sampledPosition, sampledRotation);
    }

    private void CacheVisualTransform(int index, Transform instance)
    {
        Transform visual = instance.childCount > 0 ? instance.GetChild(0) : null;
        visualTransforms[index] = visual;
        verticalOffsetPhases[index] = Random.value;
        horizontalOffsetPhases[index] = Random.value;

        if (visual == null)
        {
            visualBaseLocalPositions[index] = Vector3.zero;
            visualBaseLocalScales[index] = Vector3.one;
            return;
        }

        visualBaseLocalPositions[index] = visual.localPosition;
        visualBaseLocalScales[index] = visual.localScale;
        ApplyVisualTransform(index, GetElapsedTime());
    }

    private void UpdateVisualTransforms(float elapsedTime)
    {
        if (visualTransforms == null)
        {
            return;
        }

        for (int index = 0; index < visualTransforms.Length; index++)
        {
            ApplyVisualTransform(index, elapsedTime);
        }
    }

    private void ApplyVisualTransform(int index, float elapsedTime)
    {
        if (visualTransforms == null
            || visualBaseLocalPositions == null
            || visualBaseLocalScales == null
            || index < 0
            || index >= visualTransforms.Length
            || spawnedTransforms == null
            || index >= spawnedTransforms.Length
            || spawnedTransforms[index] == null
            || visualTransforms[index] == null)
        {
            return;
        }

        float scale = GetIndexScale(index);
        float verticalOffset = GetVerticalOffset(index, elapsedTime);
        float horizontalOffset = GetHorizontalOffset(index, elapsedTime);
        Transform visual = visualTransforms[index];
        Transform instanceTransform = spawnedTransforms[index];
        Vector3 worldOffset = instanceTransform.up * verticalOffset + instanceTransform.right * horizontalOffset;
        Vector3 localOffset = visual.parent != null
            ? visual.parent.InverseTransformVector(worldOffset)
            : worldOffset;

        visual.localScale = visualBaseLocalScales[index] * scale;
        visual.localPosition = visualBaseLocalPositions[index] + localOffset;
    }

    private float GetIndexScale(int index)
    {
        if (scaleCurve == null)
        {
            return 1f;
        }

        float normalizedIndex = instanceCount > 1
            ? Mathf.Clamp01(index / (float)(instanceCount - 1))
            : 0f;

        return Mathf.Max(0f, scaleCurve.Evaluate(normalizedIndex));
    }

    private float GetVerticalOffset(int index, float elapsedTime)
    {
        if (verticalOffsetDistance <= 0f || verticalOffsetSpeed <= 0f || verticalOffsetPhases == null)
        {
            return 0f;
        }

        float phase = index < verticalOffsetPhases.Length ? verticalOffsetPhases[index] : 0f;
        float cycle = Mathf.Repeat(elapsedTime * verticalOffsetSpeed + phase, 1f);
        float value = Mathf.Sin(cycle * Mathf.PI * 2f);
        return value * verticalOffsetDistance;
    }

    private float GetHorizontalOffset(int index, float elapsedTime)
    {
        if (horizontalOffsetDistance <= 0f || horizontalOffsetSpeed <= 0f || horizontalOffsetPhases == null)
        {
            return 0f;
        }

        float phase = index < horizontalOffsetPhases.Length ? horizontalOffsetPhases[index] : 0f;
        float cycle = Mathf.Repeat(elapsedTime * horizontalOffsetSpeed + phase, 1f);
        float value = Mathf.Sin(cycle * Mathf.PI * 2f);
        return value * horizontalOffsetDistance;
    }

    private void AddHistorySample(Vector3 position, Quaternion rotation, float distance)
    {
        if (historyPositions == null || historyPositions.Length == 0)
        {
            return;
        }

        if (historyCount == historyPositions.Length)
        {
            historyStart = (historyStart + 1) % historyPositions.Length;
            historyCount--;
        }

        int insertIndex = GetBufferIndex(historyCount);
        historyPositions[insertIndex] = position;
        historyRotations[insertIndex] = rotation;
        historyDistances[insertIndex] = distance;
        historyCount++;
    }

    private void SamplePose(float distance, ref int upperBound, out Vector3 position, out Quaternion rotation)
    {
        if (historyCount <= 0)
        {
            position = transform.position;
            rotation = transform.rotation;
            return;
        }

        if (historyCount == 1)
        {
            int onlyIndex = GetBufferIndex(0);
            position = historyPositions[onlyIndex];
            rotation = historyRotations[onlyIndex];
            return;
        }

        float oldestDistance = GetHistoryDistance(0);
        if (distance <= oldestDistance)
        {
            int oldestIndex = GetBufferIndex(0);
            position = historyPositions[oldestIndex];
            rotation = historyRotations[oldestIndex];
            return;
        }

        int latestLogicalIndex = historyCount - 1;
        float latestDistance = GetHistoryDistance(latestLogicalIndex);
        if (distance >= latestDistance)
        {
            int latestBufferIndex = GetBufferIndex(latestLogicalIndex);
            position = historyPositions[latestBufferIndex];
            rotation = historyRotations[latestBufferIndex];
            return;
        }

        upperBound = Mathf.Clamp(upperBound, 1, latestLogicalIndex);

        while (upperBound > 0 && GetHistoryDistance(upperBound - 1) >= distance)
        {
            upperBound--;
        }

        int lowerBound = upperBound - 1;
        float lowerDistance = GetHistoryDistance(lowerBound);
        float upperDistance = GetHistoryDistance(upperBound);
        int lowerIndex = GetBufferIndex(lowerBound);
        int upperIndex = GetBufferIndex(upperBound);

        if (Mathf.Abs(upperDistance - lowerDistance) <= 0.0001f)
        {
            position = historyPositions[upperIndex];
            rotation = historyRotations[upperIndex];
            return;
        }

        float lerpValue = Mathf.InverseLerp(lowerDistance, upperDistance, distance);
        position = Vector3.LerpUnclamped(historyPositions[lowerIndex], historyPositions[upperIndex], lerpValue);
        rotation = Quaternion.SlerpUnclamped(historyRotations[lowerIndex], historyRotations[upperIndex], lerpValue);
    }

    private float GetHistoryDistance(int logicalIndex)
    {
        return historyDistances[GetBufferIndex(logicalIndex)];
    }

    private int GetBufferIndex(int logicalIndex)
    {
        return (historyStart + logicalIndex) % historyPositions.Length;
    }
}
